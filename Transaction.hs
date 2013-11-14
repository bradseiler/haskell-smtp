module Transaction where
import Control.Concurrent.MVar
import Monad
import System.IO
import Text.ParserCombinators.Parsec hiding (anyToken)
import Time
import qualified State as S
import qualified Command as C
import Command(crlf, anyToken)
import Reply

parseData :: Parser (Maybe String)
parseData = try (char '.' >> crlf >> return Nothing) <|>
            ((try (char '.' >> (manyTill anyToken crlf)) <|>
                  manyTill anyToken crlf)
             >>= (\s -> return $ Just (s ++ "\r\n")))

readData :: Handle -> IO (Either ParseError [String])
readData h = do line <- getFullLine h
                case (parse parseData "" line) of
                  Left err -> return $ Left err
                  Right ret ->
                      case ret of 
                        Nothing -> return $ Right []
                        Just text ->
                            do tl <- readData h
                               case tl of
                                 Left err -> return $ Left err
                                 Right tail -> return $ Right (text:tail)

parseError :: SmtpReply
parseError = Reply 501 (hostname ++ " Parse error\r\n")

readWithState :: Handle -> MVar Handle -> MVar Handle ->
                 S.SmtpState -> IO UpdatedSmtpState
readWithState h logmv outmv (S.DataReady name from to) =
    do msg <- readData h
       (takeMVar logmv
        >>= (\log -> hPutStrLn log ("got message: " ++ show msg)
                     >> putMVar logmv log))
       ret <- case msg of
                Left _ -> return (parseError, (S.Rcpt name from to))
                Right strs -> return (dataReceived (S.DataReady name from to)
                                                   strs)
       (takeMVar logmv
        >>= (\log -> hPutStrLn log ("returning: " ++ show (fst ret))
                     >> putMVar logmv log))
       (takeMVar outmv
        >>= (\out -> getClockTime
                     >>= ((liftM calendarTimeToString) . toCalendarTime)
                     >>= (hPutStr out . (S.printData $ snd ret))
                     >> hFlush out
                     >> putMVar outmv out))
       return ret
readWithState h logmv outmv state =
    do line <- getFullLine h
       cmd <- return $ parse C.parseSmtpCmd "" line
       (takeMVar logmv
        >>= (\log -> hPutStrLn log ("got command: " ++ show cmd)
                     >> putMVar logmv log))
       ret <- case cmd of
          Left _ -> return (parseError, state)
          Right cmd ->
              case cmd of
                C.Helo name -> return $ helo state name
                C.Mail addr -> return $ mail state addr
                C.Rcpt addr -> return $ rcpt state addr
                C.Data -> return $ dataComing state
                C.Rset -> return $ rset state
                C.Send _ -> return $ unknown state
                C.Soml _ -> return $ unknown state
                C.Saml _ -> return $ unknown state
                C.Vrfy _ -> return $ unknown state
                C.Expn _ -> return $ unknown state
                C.Help _ -> return $ unknown state
                C.Noop -> return $ noop state
                C.Quit -> return $ quit state
                C.Turn -> return $ unknown state
       (takeMVar logmv
        >>= (\log -> hPutStrLn log ("returning: " ++ show (fst ret))
                     >> putMVar logmv log))
       return ret
                        
transThreadStep :: Handle -> MVar Handle -> MVar Handle -> 
                   UpdatedSmtpState -> IO ()
transThreadStep h logmv outmv (rep, st) =
    (hPutStr h $ show rep) >> (case (code rep) of
                                 221 -> (putStrLn $ show st)
                                        >> takeMVar logmv
                                        >>= (\log -> hFlush log
                                                     >> putMVar logmv log)
                                        >> return ()
                                 _ -> readWithState h logmv outmv st
                                      >>= transThreadStep h logmv outmv)

transThread :: Handle -> MVar Handle -> MVar Handle -> IO ()
transThread h logmv outmv = do hSetBuffering h LineBuffering
                               transThreadStep h logmv outmv open
                               hClose h

getFullLine :: Handle -> IO String
getFullLine h = do c <- hGetChar h
                   case c of
                     '\r' -> do next <- hGetChar h
                                case next of
                                  '\n' -> return "\r\n"
                                  _ -> (liftM (('\r':). (c:))) $ getFullLine h
                     _ -> (liftM (c:)) $ getFullLine h