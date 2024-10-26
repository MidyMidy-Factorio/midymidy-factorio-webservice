{-# LANGUAGE BlockArguments, DeriveAnyClass #-}

module Rcon
  ( auth
  , sendCommand
  , Mux
  , multiplex
  , execCommand
  , closeMultiplexer
  ) where

import Control.Concurrent (forkIO, ThreadId, killThread)
import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, takeMVar, putMVar, modifyMVar, modifyMVar_)
import Control.Exception (Exception, catch, throwIO, assert, SomeException)
import Control.Monad (guard, forever, when)
import Data.Maybe (isNothing)
import qualified Data.Binary as Bin
import qualified Data.Binary.Get as Bin
import qualified Data.Binary.Put as Bin
import qualified Data.ByteString.Lazy as BS
import qualified Data.IntMap as IntMap
import System.IO (Handle)

data Mux = Mux
  { muxHandle :: Handle
  , muxReceriverTid :: ThreadId
  , muxState :: MVar MuxState
  }

data MuxState = MuxState
  { muxCounter :: Int
  , muxRegistry :: Registry
  , muxDead :: Maybe SomeException
  }

type Registry = IntMap.IntMap (MVar BS.ByteString)

data MuxDead = MuxDead SomeException deriving (Show, Exception)

multiplex :: Handle -> IO Mux
multiplex handle = do
  state <- newMVar (MuxState 0 IntMap.empty Nothing)
  tid <- forkIO . flip catch (\(e :: SomeException) -> modifyMVar_ state (\(MuxState c r _) -> pure $ MuxState c r $ Just e)) . forever $ do
    GnrResponse mId body <- recvResponse handle
    reply <- modifyMVar state \(MuxState ctr reg dead) -> assert (isNothing dead) $ pure (MuxState ctr (IntMap.delete mId reg) Nothing, IntMap.lookup mId reg)
    case reply of
      Just m -> putMVar m body
      Nothing -> putStrLn $ "Recieved spurious rcon message id " <> show mId
  pure (Mux handle tid state)

execCommand :: Mux -> BS.ByteString -> IO BS.ByteString
execCommand mux cmd = do
  reply <- newEmptyMVar
  mId <- modifyMVar (muxState mux) \(MuxState ctr reg dead) -> do
    case dead of
      Just e -> throwIO $ MuxDead e
      Nothing -> pure (MuxState (ctr + 1) (IntMap.insert ctr reply reg) Nothing, ctr)
  sendRequest (muxHandle mux) (ExecCmdRequest mId cmd)
  takeMVar reply

closeMultiplexer :: Mux -> IO ()
closeMultiplexer mux = killThread (muxReceriverTid mux)

sendRequest :: Handle -> Request -> IO ()
sendRequest handle req = BS.hPut handle (Bin.encode req)

recvResponse :: Handle -> IO Response
recvResponse handle = do
  sizeBs <- BS.hGet handle 4
  let size = fromIntegral (Bin.runGet Bin.getInt32le sizeBs)
  remBs <- BS.hGet handle size
  pure (Bin.decode (BS.append sizeBs remBs))

auth :: Handle -> BS.ByteString -> IO Bool
auth handle passwd = do
  sendRequest handle (AuthRequest 1 passwd)
  AuthResponse status <- recvResponse handle
  guard (status == 1 || status == -1)
  pure (status == 1)

sendCommand :: Handle -> BS.ByteString -> IO BS.ByteString
sendCommand handle cmd = do
  sendRequest handle (ExecCmdRequest 2 cmd)
  GnrResponse 2 res <- recvResponse handle
  pure res

data Request
  = AuthRequest
    { authReqId :: Int
    , authReqPassword :: BS.ByteString
    }
  | ExecCmdRequest
    { execCmdReqId :: Int
    , execCmdReqCmd :: BS.ByteString
    }
  deriving (Show)

data Response
  = AuthResponse
    { authResId :: Int }
  | GnrResponse
    { gnrResId :: Int
    , gnrResBody :: BS.ByteString
    }
  deriving (Show)

instance Bin.Binary Request where
  get = do
    size <- getInt
    mId <- getInt
    t <- getInt
    body <- Bin.getLazyByteString (fromIntegral (size - 10))
    getNull
    getNull
    case t of
      3 -> pure (AuthRequest mId body)
      2 -> pure (ExecCmdRequest mId body)
      _ -> fail $ "unknown request type " <> show t
  put (AuthRequest mId passwd) = do
    putInt (fromIntegral (BS.length passwd) + 10)
    putInt mId
    putInt 3
    Bin.putLazyByteString passwd
    Bin.putWord8 0
    Bin.putWord8 0
  put (ExecCmdRequest mId cmd) = do
    putInt (fromIntegral (BS.length cmd) + 10)
    putInt mId
    putInt 2
    Bin.putLazyByteString cmd
    Bin.putWord8 0
    Bin.putWord8 0

instance Bin.Binary Response where
  get = do
    size <- getInt
    mId <- getInt
    t <- getInt
    body <- Bin.getLazyByteString (fromIntegral (size - 10))
    getNull
    getNull
    case t of
      2 -> pure (AuthResponse mId)
      0 -> pure (GnrResponse mId body)
      _ -> fail $ "unknown response " <> show t
  put (AuthResponse mId) = do
    putInt 10
    putInt mId
    putInt 2
    Bin.putWord8 0
    Bin.putWord8 0
  put (GnrResponse mId body) = do
    putInt (fromIntegral (BS.length body) + 10)
    putInt mId
    putInt 0
    Bin.putLazyByteString body
    Bin.putWord8 0
    Bin.putWord8 0

getInt :: Bin.Get Int
getInt = fromIntegral <$> Bin.getInt32le

putInt :: Int -> Bin.Put
putInt = Bin.putInt32le . fromIntegral

getNull :: Bin.Get ()
getNull = do
  w <- Bin.getWord8
  when (w /= 0) $ fail "expected '\0'"
