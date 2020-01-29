module Main (main) where

import Control.Exception (throwIO)
import Network.Socket (socket, Family(AF_INET), SocketType(Stream), defaultProtocol, connect, SockAddr(SockAddrInet), tupleToHostAddress, socketToHandle)
import Network.Wai.Handler.Warp (run)
import qualified Data.ByteString.Lazy.UTF8 as BSU
import System.Environment (getArgs)
import System.IO.Error (userError)
import System.IO (Handle, IOMode(ReadWriteMode))

import Rcon (auth, sendCommand)
import WebService (app)

main = do
    [port, password] <- getArgs
    app (rcon port password) >>= run 8081
    where
    rcon port password cmd = do
        sock <- socket AF_INET Stream defaultProtocol
        connect sock (SockAddrInet (read port) (tupleToHostAddress (127, 0, 0, 1)))
        handle <- socketToHandle sock ReadWriteMode
        success <- auth handle (BSU.fromString password)
        if success
            then sendCommand handle cmd
            else throwIO (userError "Authentication failed")
