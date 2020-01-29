module WebService
    ( app
    ) where

import Control.Monad.IO.Class (liftIO)
import Network.Wai (Application)
import System.IO (Handle)
import Web.Scotty (ScottyM, scottyApp, get, raw, text, setHeader)

import Rcon (sendCommand)

app :: Handle -> IO Application
app rcon = scottyApp do
    get "/update" do
        setHeader "Content-Type" "application/json; charset=utf-8"
        liftIO (sendCommand rcon "/_midymidyws get_update")
            >>= raw
    get "/players" do
        setHeader "Content-Type" "application/json; charset=utf-8"
        liftIO (sendCommand rcon "/_midymidyws get_players")
            >>= raw
