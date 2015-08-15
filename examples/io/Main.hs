{-# LANGUAGE Arrows #-}

module Main where

import FWGL
import FWGL.Backend.GLFW.GL20
import FWGL.Graphics.D2

import Control.Concurrent
import Control.Monad (forever, when)
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Data.IORef
import Network.Socket
import Network.Socket.ByteString.Lazy as N
import System.Environment

otherPos :: SF (Input String) (Int, Int)
otherPos = custom >>^ read

mainSF :: (String -> IO ()) -> SF (Input String) Output
mainSF change = proc inp ->
        do me <- pointer -< inp
           other <- otherPos -< inp

           meChanged <- edgeBy (\prev next -> if prev == next
                                                  then Nothing
                                                  else Just next) (0, 0) -< me

           returnA -< drawEff [elementsScreen [ drawQuad me
                                              , drawQuad other ]]
                              (liftIO $ event (return ()) change $ fmap show meChanged)

main :: IO ()
main = do (out, inp, close) <- createConnection
          fwgl . run' inp $ mainSF out
          close

createConnection :: IO (String -> IO (), IO String, IO ())
createConnection =
        do sendVar <- newEmptyMVar
           recvRef <- newIORef $ show (0, 0)
           sock <- socket AF_INET Stream defaultProtocol
           forkIO $ getArgs >>= \args -> case args of
                   (host : portStr : []) -> client sock sendVar recvRef
                                                   host $ read portStr
                   (portStr : []) -> server sock sendVar recvRef $ read portStr
                   _ -> error "Invalid number of arguments."
           return (putMVar sendVar, readIORef recvRef, close sock)

server :: Socket -> MVar String -> IORef String -> Integer -> IO ()
server sock sendVar recvRef portNum =
        do addr <- inet_addr "0.0.0.0"
           bind sock $ SockAddrInet (fromInteger portNum) addr
           listen sock 1
           putStrLn $ concat ["Listening to 0.0.0.0:", show portNum, "..."]
           forever $ accept sock >>= \(other, addr) ->
                do putStrLn $ concat ["Connected to ", show addr, "."]
                   sendAndRecv other sendVar recvRef

client :: Socket -> MVar String -> IORef String -> String -> Integer -> IO ()
client sock sendVar recvRef hostName portNum = forever $
        do addr <- fmap (SockAddrInet $ fromInteger portNum)
                        $ inet_addr hostName
           connect sock addr
           putStrLn $ concat ["Connected to ", show addr, "."]
           sendAndRecv sock sendVar recvRef

sendAndRecv :: Socket -> MVar String -> IORef String -> IO ()
sendAndRecv sock sendVar recvRef =
        do sendThread <- forkIO sendLoop
           recvLoop sendThread
        where sendLoop = do str <- takeMVar sendVar
                            sendAll sock . pack $ str ++ "\n"
                            sendLoop

              recvLoop sendThread =
                      do msgs <- fmap (lines . unpack) $ N.getContents sock
                         putStrLn "proc msgs"
                         flip mapM_ msgs $ \str -> do writeIORef recvRef str
                                                      putStrLn str
                         killThread sendThread

              {-
              sendStr str =
                      do sent <- send sock str
                         case () of _ | sent < 0 -> return False
                                    _ | sent < length str ->
                                            sendStr $ drop sent str
                                    _ -> return True
              -}

drawQuad :: (Int, Int) -> Element
drawQuad (x, y) = pos (Vec2 (fromIntegral x - 320)
                            (- fromIntegral y + 240)) $
                  rect (Vec2 10 10) (colorTex red)
