module WebService
    ( app
    ) where

import Control.Monad.IO.Class (liftIO)
import Network.Wai (Application)
import System.IO (Handle)
import Web.Scotty (ScottyM, scottyApp, get, raw, text, setHeader)
import Data.ByteString.Lazy (ByteString)

app :: (ByteString -> IO ByteString) -> IO Application
app rcon = scottyApp do
    get "/update" do
        setHeader "Content-Type" "application/json; charset=utf-8"
        liftIO (rcon "/_midymidyws get_update") >>= raw
    get "/players" do
        setHeader "Content-Type" "application/json; charset=utf-8"
        liftIO (rcon "/_midymidyws get_players") >>= raw
