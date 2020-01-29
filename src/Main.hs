module Main (main) where

import Network.Wai.Handler.Warp (run)
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy.UTF8 as BSU
import System.IO (Handle, IOMode(ReadWriteMode))
import Network.Socket (socket, Family(AF_INET), SocketType(Stream), defaultProtocol, connect, SockAddr(SockAddrInet), tupleToHostAddress, socketToHandle)
import Rcon (auth)
import WebService (app)

main = do
    [port, password] <- getArgs
    sock <- socket AF_INET Stream defaultProtocol
    connect sock (SockAddrInet (read port) (tupleToHostAddress (127, 0, 0, 1)))
    handle <- socketToHandle sock ReadWriteMode
    success <- auth handle (BSU.fromString password)
    if success
        then app handle >>= run 8081
        else putStrLn "Authentication failed."
