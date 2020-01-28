module WebService
    ( app
    ) where

import Control.Concurrent.STM.TChan (TChan, tryReadTChan)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (atomically)
import Data.ByteString.Lazy (ByteString)
import Network.Wai (Application)
import Web.Scotty (ScottyM, scottyApp, get, raw, text, setHeader)

app :: TChan ByteString -> IO Application
app updates = scottyApp do
    get "/update" do
        setHeader "Content-Type" "application/json; charset=utf-8"
        update <- liftIO . atomically . tryReadTChan $ updates
        case update of
            Just json -> do
                raw json
            Nothing -> do
                raw "{type:\"empty\"}"
