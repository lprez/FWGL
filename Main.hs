import Control.Monad
import Data.Char
import System.IO
import System.IO.Error
import Network
import Network.BSD
import Network.Socket
import System.Environment
import System.Process

main = do [host,port] <- getArgs
          sock <- socket AF_INET Stream defaultProtocol
          setSocketOption sock ReuseAddr 1
          inet <- inet_addr host
          bind sock $ SockAddrInet (fromInteger $ read port) inet
          listen sock maxListenQueue
          packages <- readProcess "ghc-pkg" ["list", "--simple-output"] []
          let response = "HTTP/1.1 200 OK\r\n\r\nWelcome to Haskell Cloud! The following packages are pre-installed:\n\n" ++ unlines (words packages)
          forever $ do (handle,_,_) <- Network.accept sock
                       request <- hGetContents handle
                       when (any (null . dropWhile isSpace) (lines request)) $ void $ tryIOError $ hPutStr handle response
                       hClose handle
