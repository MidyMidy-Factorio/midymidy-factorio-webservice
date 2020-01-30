module Rcon
    ( auth
    , sendCommand
    , MuxConn
    , multiplex
    , execCommand
    , closeMultiplexer
    ) where

import System.IO (Handle)
import Data.ByteString.Lazy (ByteString)
import Data.Binary (Binary(get, put), Get, getWord8, Put, putWord8, encode, decode)
import Data.Binary.Get (runGet, getInt32le, getLazyByteString)
import Data.Binary.Put (putInt32le, putLazyByteString)
import Data.Function (fix)
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, putMVar)
import Control.Monad (guard)
import qualified Data.ByteString.Lazy as BS

type MuxConn = Chan (Maybe (ByteString, MVar ByteString))

multiplex :: Handle -> IO MuxConn
multiplex handle = do
    requests <- newChan
    replies <- newChan
    forkIO . fix $ \loop -> do
        req <- readChan requests
        case req of
            Just (cmd, reply) -> do
                sendRequest handle (ExecCmdRequest 2 cmd)
                writeChan replies (Just reply)
                loop
            Nothing -> writeChan replies Nothing
    forkIO . fix $ \loop -> do
        reply <- readChan replies
        case reply of
            Just rep -> do
                GnrResponse 2 res <- recvResponse handle
                putMVar rep res
                loop
            Nothing -> return ()
    return requests

execCommand :: MuxConn -> ByteString -> IO ByteString
execCommand conn cmd = do
    reply <- newEmptyMVar
    writeChan conn (Just (cmd, reply))
    takeMVar reply

closeMultiplexer :: MuxConn -> IO ()
closeMultiplexer conn = writeChan conn Nothing

sendRequest :: Handle -> Request -> IO ()
sendRequest handle req = BS.hPut handle (encode req)

recvResponse :: Handle -> IO Response
recvResponse handle = do
    sizeBs <- BS.hGet handle 4
    let size = fromIntegral (runGet getInt32le sizeBs)
    remBs <- BS.hGet handle size
    return (decode (BS.append sizeBs remBs))

auth :: Handle -> ByteString -> IO Bool
auth handle passwd = do
    sendRequest handle (AuthRequest 1 passwd)
    AuthResponse status <- recvResponse handle
    guard (status == 1 || status == -1)
    return (status == 1)

sendCommand :: Handle -> ByteString -> IO ByteString
sendCommand handle cmd = do
    sendRequest handle (ExecCmdRequest 2 cmd)
    GnrResponse 2 res <- recvResponse handle
    return res

data Request
    = AuthRequest
        { authReqId :: Int
        , authReqPassword :: ByteString
        }
    | ExecCmdRequest
        { execCmdReqId :: Int
        , execCmdReqCmd :: ByteString
        }
    deriving (Show)

data Response
    = AuthResponse
        { authResId :: Int }
    | GnrResponse
        { gnrResId :: Int
        , gnrResBody :: ByteString
        }
    deriving (Show)

instance Binary Request where
    get = do
        size <- getInt
        id <- getInt
        t <- getInt
        body <- getLazyByteString (fromIntegral (size - 10))
        getNull
        getNull
        case t of
            3 -> return (AuthRequest id body)
            2 -> return (ExecCmdRequest id body)
    put (AuthRequest id passwd) = do
        putInt (fromIntegral (BS.length passwd) + 10)
        putInt id
        putInt 3
        putLazyByteString passwd
        putWord8 0
        putWord8 0
    put (ExecCmdRequest id cmd) = do
        putInt (fromIntegral (BS.length cmd) + 10)
        putInt id
        putInt 2
        putLazyByteString cmd
        putWord8 0
        putWord8 0

instance Binary Response where
    get = do
        size <- getInt
        id <- getInt
        t <- getInt
        body <- getLazyByteString (fromIntegral (size - 10))
        getNull
        getNull
        case t of
            2 -> return (AuthResponse id)
            0 -> return (GnrResponse id body)
    put (AuthResponse id) = do
        putInt 10
        putInt id
        putInt 2
        putWord8 0
        putWord8 0
    put (GnrResponse id body) = do
        putInt (fromIntegral (BS.length body) + 10)
        putInt id
        putInt 0
        putLazyByteString body
        putWord8 0
        putWord8 0

getInt :: Get Int
getInt = fromIntegral <$> getInt32le

putInt :: Int -> Put
putInt = putInt32le . fromIntegral

getNull :: Get ()
getNull = do
    w <- getWord8
    if w /= 0
        then fail "expected '\0'"
        else return ()
