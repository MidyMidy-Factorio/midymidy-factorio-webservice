{-# LANGUAGE BlockArguments, OverloadedStrings, DeriveGeneric, DeriveAnyClass, NumericUnderscores #-}

module Main ( main ) where

import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay, forkIO)
import Control.Exception (throwIO, catch, SomeException)
import Control.Lens (folded, maximumOf, (.~), (^.), (^..), (^?), (^?!))
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when, forever)
import Data.Aeson.Lens (key, values, _Array, _Bool, _Integer, _String)
import Data.ByteString.Lazy (ByteString)
import Data.List (isPrefixOf)
import Data.Maybe (fromJust, fromMaybe)
import Data.Monoid (First(..))
import GHC.Generics (Generic)
import Network.HTTP.Simple (httpBS, parseRequest_, setRequestQueryString)
import Network.Socket (socket, Family(AF_INET), SocketType(Stream), defaultProtocol, connect, SockAddr(SockAddrInet), tupleToHostAddress, socketToHandle, PortNumber)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy.UTF8 as ByteString
import qualified Data.ByteString.UTF8 as StrictByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Rcon (auth, multiplex, execCommand)
import System.Environment (getEnv)
import System.IO.Error (userError)
import System.IO (Handle, IOMode(ReadWriteMode))
import System.IO.Unsafe (unsafePerformIO)
import Web.Scotty (scottyApp, get, post, body, raw, setHeader, raise)

{-# NOINLINE chatId #-}
chatId :: Integer
chatId = read . unsafePerformIO . getEnv $ "TGBOT_CHAT_ID"

{-# NOINLINE botToken #-}
botToken :: String
botToken = unsafePerformIO . getEnv $ "TGBOT_TOKEN"

{-# NOINLINE rconPort #-}
rconPort :: PortNumber
rconPort = read . unsafePerformIO . getEnv $ "RCON_PORT"

{-# NOINLINE rconPassword #-}
rconPassword :: String
rconPassword = read . unsafePerformIO . getEnv $ "RCON_PASSWORD"

botURL :: String
botURL = "https://api.telegram.org/bot" ++ botToken

tg2f :: (ByteString -> IO ByteString) -> IO Application
tg2f rcon = scottyApp do
  post "/" $ do
    setHeader "Content-Type" "application/json; charset=utf-8"
    json <- fromJust . Json.decode <$> body
    onUpdate (json :: Json.Value)
  where
    onUpdate update = maybe (pure ()) onMessage $ update ^? key "message"
    onMessage msg = do
      let mid = msg ^?! key "message_id" . _Integer
          cid = msg ^?! key "chat" . key "id" . _Integer
          name = msg ^?! key "from" . key "first_name" . _String
      when (cid == chatId) . maybe (pure ()) (onText cid mid name) $ msg ^? key "text" . _String <|> msg ^? key "caption" . _String
    onText cid mid name text = fromMaybe (pure ()) . getFirst . ($ (text, mid, cid, name)) . mconcat . (fmap . fmap $ First) $ handlers
    handlers = [ online, chat ]
    online (text, cid, mid, _) = if not ("/online" `Text.isPrefixOf` text) then Nothing else Just $ do
      msg <- liftIO (rcon "/_midymidyws get_players")
      raw . Json.encode $ Response "sendMessage" (ByteString.toString msg) cid (Just mid)
    chat (msg, _, _, name) = Just $ do
      let msgs = PostMessages [ Message name msg ]
      liftIO $ rcon ("/_midymidyws post_messages " <> Json.encode msgs)
      pure ()

data Response = Response
  { method :: String
  , text :: String
  , chat_id :: Integer
  , reply_to_message_id :: Maybe Integer
  }
  deriving (Generic, Json.ToJSON)

data PostMessages = PostMessages
  { messages :: [Message] }
  deriving (Generic, Show, Json.ToJSON)

data Message = Message
  { name :: Text.Text
  , message :: Text.Text
  }
  deriving (Generic, Show, Json.ToJSON)

f2tg :: (ByteString -> IO ByteString) -> IO ()
f2tg rcon = forever $ do
  threadDelay 200_000
  update <- fromJust . Json.decode <$> rcon "/get_update"
  case Text.unpack $ (update :: Json.Value) ^?! key "type" . _String of
    "console-chat" -> do
      let name = update ^?! key "player_name" . _String
          msg = update ^?! key "message" . _String
          text = "<" <> name <> ">: " <> msg
      httpBS
        . setRequestQueryString
          [ ("chat_id", Just . StrictByteString.fromString . show $ chatId)
          , ("text", Just . Text.encodeUtf8 $ text)
          ]
        . parseRequest_
        $ botURL <> "/sendMessage"
      pure ()
    _ -> pure ()

main :: IO ()
main = do
  sock <- socket AF_INET Stream defaultProtocol
  connect sock (SockAddrInet rconPort (tupleToHostAddress (127, 0, 0, 1)))
  handle <- socketToHandle sock ReadWriteMode
  success <- auth handle (ByteString.fromString rconPassword)
  when (not success) (throwIO (userError "Authentication failed"))
  conn <- multiplex handle
  forkIO . forever $ (tg2f (execCommand conn) >>= run 8085) `catch` printE
  forkIO . forever $ f2tg (execCommand conn) `catch` printE
  pure ()
  where
    printE :: SomeException -> IO ()
    printE = print . show

