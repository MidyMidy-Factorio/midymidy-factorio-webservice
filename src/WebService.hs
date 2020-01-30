module WebService
    ( app
    ) where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy (ByteString)
import Network.Wai (Application)
import Web.Scotty (scottyApp, get, post, body, raw, setHeader, raise)
import GHC.Generics (Generic)
import qualified Data.Aeson as Json
import qualified Data.Text

app :: (ByteString -> IO ByteString) -> IO Application
app rcon = scottyApp do
    get "/update" . useRcon . return $
        "/_midymidyws get_update"
    get "/players" . useRcon . return $
        "/_midymidyws get_players"
    post "/messages" . useRcon $ do
        json <- body
        case Json.decode json :: Maybe WSPostMessagesBody of
            Just _ ->
                return ("/_midymidyws post_messages " <> json)
            Nothing ->
                raise "POST /messages: Wrong format."
    where
    useRcon buildCmd = do
        cmd <- buildCmd
        setHeader "Content-Type" "application/json; charset=utf-8"
        liftIO (rcon cmd) >>= raw

data WSPostMessagesBody = WSPostMessagesBody
    { messages :: [WSMessage] }
    deriving (Generic, Show, Json.FromJSON)
data WSMessage = WSMessage
    { name :: Data.Text.Text
    , message :: Data.Text.Text
    }
    deriving (Generic, Show, Json.FromJSON)
