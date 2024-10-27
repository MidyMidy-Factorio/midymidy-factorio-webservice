{-# LANGUAGE BlockArguments, OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module Main (main) where

import Control.Concurrent (threadDelay, forkIO, killThread, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (throwIO, catch, SomeException, bracket, Exception)
import Control.Lens ((^?), (^?!), (?~), _Just)
import Control.Monad.Fix (mfix)
import Control.Monad (forever, unless, void, forM_, (<=<))
import Data.Aeson.Lens (key, _Integer, _String)
import Data.Function ((&))
import Data.Maybe (fromJust, fromMaybe)
import GHC.Generics (Generic)
import Network.Socket (socket, Family(AF_INET), SocketType(Stream), defaultProtocol, connect, SockAddr(SockAddrInet), tupleToHostAddress, socketToHandle, PortNumber)
import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.UTF8 as BS
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Data.Map.Strict as Map
import qualified Network.Matrix.Client as Mx
import qualified Network.Matrix.Client.Lens as Mx
import Rcon (auth, multiplex, execCommand, closeMultiplexer)
import System.Environment (getEnv)
import System.IO (IOMode(ReadWriteMode))
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)
import Data.IORef (readIORef, newIORef, writeIORef)
import Data.Traversable (for)

{-# NOINLINE homeServer#-}
homeServer :: Text.Text
homeServer = Text.pack . unsafePerformIO . getEnv $ "HOME_SERVER"

{-# NOINLINE matrixRoom#-}
matrixRoom :: Text.Text
matrixRoom = Text.pack . unsafePerformIO . getEnv $ "MATRIX_ROOM"

{-# NOINLINE rconPort #-}
rconPort :: PortNumber
rconPort = read . unsafePerformIO . getEnv $ "RCON_PORT"

{-# NOINLINE rconPassword #-}
rconPassword :: String
rconPassword = unsafePerformIO . getEnv $ "RCON_PASSWORD"

unwrapMxError :: Either Mx.MatrixError a -> IO a
unwrapMxError = either (throwIO . WrapMatrixError) pure

newtype MatrixException = WrapMatrixError Mx.MatrixError deriving Show
instance Exception MatrixException where

data UnsafeUser = UnsafeUser { unsafeUserDisplayName :: Maybe Text.Text, unsafeUserAvatarUrl :: Maybe Text.Text }

mx2f :: Mx.ClientSession -> (BS.ByteString -> IO BS.ByteString) -> IO ()
mx2f session rcon = do
  userId <- unwrapMxError =<< Mx.getTokenOwner session
  filterId <- unwrapMxError =<< Mx.createFilter session userId (Mx.messageFilter & Mx._filterRoom . _Just . Mx._rfTimeline . _Just . Mx._refRooms ?~ [matrixRoom])
  members <- newIORef =<< unwrapMxError =<< Mx.getRoomMembers session (Mx.RoomID matrixRoom)
  unwrapMxError =<< Mx.syncPoll session (Just filterId) Nothing (Just Mx.Online) \syncRes ->
    forM_ (Mx.getTimelines syncRes) \(_, events) -> do
      let Mx.UserID userIdText = userId
      messages <- (concat <$>) . for (NonEmpty.toList events) $ \event -> do
        let uid = Mx.unAuthor . Mx.reSender $ event
        name <- fromMaybe uid . ((unsafeUserDisplayName . unsafeCoerce) <=< Map.lookup (Mx.UserID uid)) <$> readIORef members
        case event ^? Mx._reContent . Mx._EventRoomMessage . Mx._RoomMessageText . Mx._mtBody of
          Just text
            | text == "!refresh" -> do
              writeIORef members =<< unwrapMxError =<< Mx.getRoomMembers session (Mx.RoomID matrixRoom)
              pure []
            | uid /= userIdText -> pure [Message name text]
          _ -> pure []
      void . rcon $ "/_midymidyws post_messages " <> Json.encode (PostMessages messages)

data PostMessages = PostMessages
  { messages :: [Message] }
  deriving (Generic, Show, Json.ToJSON)

data Message = Message
  { name :: Text.Text
  , message :: Text.Text
  }
  deriving (Generic, Show, Json.ToJSON)

f2mx :: (BS.ByteString -> IO BS.ByteString) -> Mx.ClientSession -> IO ()
f2mx rcon session = forever $ do
  threadDelay 200_000
  update <- fromJust . Json.decode <$> rcon "/_midymidyws get_update"
  case Text.unpack $ (update :: Json.Value) ^?! key "type" . _String of
    "console-chat" -> do
      let name = update ^?! key "player_name" . _String
          msg = update ^?! key "message" . _String
          text = if msg == "!refresh" then msg else "<" <> name <> ">: " <> msg
          txnId = Text.pack $ show (update ^?! key "tick" . _Integer) <> "-" <> show (update ^?! key "event_id" . _Integer)
      void $ Mx.sendMessage session (Mx.RoomID matrixRoom) (Mx.EventRoomMessage (Mx.RoomMessageText (Mx.MessageText text Mx.TextType Nothing Nothing))) (Mx.TxnID txnId)
    _ -> pure ()

retry :: Int -> IO a -> IO a
retry n io = retry_ 0 where
  retry_ c = io `catch` \(e :: SomeException) -> do
    print e
    if c < n
      then do
        threadDelay $ (2 ^ c) * 1_000_000
        retry_ $ c + 1
      else throwIO e

race :: IO a -> IO a -> IO a
race a b = do
  result <- newEmptyMVar
  void . mfix $ \ ~(ta, tb) ->
    liftA2 (,)
      (forkIO $ (putMVar result =<< a) >> killThread tb)
      (forkIO $ (putMVar result =<< b) >> killThread ta)
  takeMVar result

main :: IO ()
main = retry 10 do
  sock <- socket AF_INET Stream defaultProtocol
  connect sock (SockAddrInet rconPort (tupleToHostAddress (127, 0, 0, 1)))
  handle <- socketToHandle sock ReadWriteMode
  success <- auth handle (BS.fromString rconPassword)
  unless success (throwIO (userError "Authentication failed"))
  matrixToken <- Mx.getTokenFromEnv "MATRIX_TOKEN"
  session <- Mx.createSession homeServer matrixToken
  bracket (multiplex handle) closeMultiplexer \conn ->
    race
      (retry 3 $ mx2f session (execCommand conn))
      (retry 3 $ f2mx (execCommand conn) session)
