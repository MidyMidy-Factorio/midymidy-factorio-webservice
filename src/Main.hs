module Main (main) where

import FifoReader (mkChanFromFifo)
import Network.Wai.Handler.Warp (run)
import System.Environment (getArgs)
import WebService (app)

main = do
    [updatePath] <- getArgs
    chan <- mkChanFromFifo updatePath 200
    app chan >>= run 8081
