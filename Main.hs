module Main where
import Control.Concurrent
import Control.Concurrent.MVar
import Monad
import Network
import System.IO
import Transaction

-- Server needs root privileges to run on server ports, otherwise use
-- large port numbers
port :: PortID
port = PortNumber 3000

main :: IO ()
main = do loghandle <- openFile "log.txt" AppendMode
          logMVar <- newMVar loghandle
          hPutStrLn loghandle "Starting current log session..."
          outhandle <- openFile "msgs.txt" AppendMode
          outMVar <- newMVar outhandle
          socket <- listenOn port
          sessionFactory socket logMVar outMVar

sessionFactory :: Socket -> MVar Handle -> MVar Handle -> IO ()
sessionFactory sock logvar outvar =
    do (hdl, host, port) <- accept sock
       log <- takeMVar logvar
       hPutStrLn log ("Received connection from " ++ host)
       putMVar logvar log
       forkIO $ transThread hdl logvar outvar
       sessionFactory sock logvar outvar