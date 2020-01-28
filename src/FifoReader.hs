module FifoReader
    ( mkChanFromFifo
    ) where

import System.IO (FilePath)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM.TChan (TChan, newTChanIO, writeTChan)
import Control.Monad (forever)
import Control.Monad.STM (atomically)

mkChanFromFifo :: FilePath -> Int -> IO (TChan ByteString)
mkChanFromFifo filepath delay = do
    chan <- newTChanIO
    forkIO . forever $ do
        contents <- BS.readFile filepath
        if BS.length contents /= 0
            then atomically . writeTChan chan $ contents
            else threadDelay delay
    return chan
