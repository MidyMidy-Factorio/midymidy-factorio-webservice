module Main (main) where

import Control.Exception (throwIO)
import Control.Monad (when)
import Network.Socket (socket, Family(AF_INET), SocketType(Stream), defaultProtocol, connect, SockAddr(SockAddrInet), tupleToHostAddress, socketToHandle)
import Network.Wai.Handler.Warp (run)
import System.Environment (getArgs)
import System.IO.Error (userError)
import System.IO (Handle, IOMode(ReadWriteMode))
import qualified Data.ByteString.Lazy.UTF8 as BSU

import Rcon (auth, multiplex, execCommand)
import WebService (app)

main = do
    [port, password] <- getArgs
    sock <- socket AF_INET Stream defaultProtocol
    connect sock (SockAddrInet (read port) (tupleToHostAddress (127, 0, 0, 1)))
    handle <- socketToHandle sock ReadWriteMode
    success <- auth handle (BSU.fromString password)
    when (not success) (throwIO (userError "Authentication failed"))
    conn <- multiplex handle
    app (execCommand conn) >>= run 8081
