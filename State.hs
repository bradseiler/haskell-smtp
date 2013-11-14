module State where
import System.IO

data SmtpState = Init
               | Helo { name :: String }
               | Mail { name :: String, from :: String }
               | Rcpt { name :: String, from :: String, to :: [String] }
               | DataReady { name :: String, from :: String, to :: [String] }
               | DataIn { name :: String, from :: String, to :: [String],
                          msg :: [String] }
instance Show SmtpState where
    show Init = "Initial state"
    show (Helo name) = "Recieved HELO from " ++ name
    show (Mail name from) = "Recieved MAIL from " ++ name ++ " from sender "
                            ++ from
    show (Rcpt name from to) = "Recieved MAIL from " ++ name ++ " from sender "
                               ++ from ++ " for user " ++ (foldl (++) [] to)
    show (DataReady name _ _) = "Waiting for data from " ++ name
    show (DataIn name _ _ _) = "Recieved data from " ++ name

printData :: SmtpState -> String -> String
printData (DataIn name from to msg) =
    \time -> ("Message received from host " ++ name ++ "\r\nTimestamp: "
              ++ time ++ "\r\nReturnPath:" ++ from
              ++ (foldl (++) "" (map ("\r\nForwardPath:"++) to)) ++ "\r\n"
              ++ (foldl (++) "" msg))
      