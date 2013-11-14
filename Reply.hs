module Reply where
import Text.ParserCombinators.Parsec
import State
import Command (localpart, domain, adl)

data SmtpReply = Reply { code :: Int, text :: String }
instance Show SmtpReply where
    show (Reply n str) = show n ++ " " ++ str

type UpdatedSmtpState = (SmtpReply, SmtpState)

-- These local variables hardcode the names that the server accepts.

hostname :: String
local_users :: [String]
hostname = "bradseiler.com"
local_users = ["brad", "webmaster", "spam", "help"]

-- Each function provides an appropriate reply to the given command.

open :: UpdatedSmtpState
open = (Reply 220 (hostname ++ " SMTP ready\r\n"), Init)

helo :: SmtpState -> String -> UpdatedSmtpState
helo _ str = (Reply 250 (hostname ++ " It is a pleasure to meet you " ++ str
                        ++ "\r\n"),
              Helo str)

mail :: SmtpState -> String -> UpdatedSmtpState
mail Init addr = (Reply 250 (hostname ++ " address " ++ addr ++ " OK\r\n"),
                  Mail "" addr)
mail state addr = (Reply 250 (hostname ++ " address " ++ addr ++ " OK\r\n"),
                   Mail (name state) addr)

rcpt_bad_order :: SmtpReply
rcpt_bad_order = Reply 503 (hostname ++ " MAIL required before RCPT\r\n")

rcpt_good_addr :: String -> SmtpReply
rcpt_good_addr str = Reply 250
                     (hostname ++ " address " ++ str ++ " is valid\r\n")

rcpt_bad_addr :: String -> SmtpReply
rcpt_bad_addr str = Reply 550
                    (hostname ++ " address " ++ str ++ " not found\r\n")

rcpt :: SmtpState -> String -> UpdatedSmtpState
rcpt Init _ = (rcpt_bad_order, Init)
rcpt (Helo name) _ = (rcpt_bad_order, Helo name)
rcpt st str = if (is_local_addr str)
              then (rcpt_good_addr str,
                    case st of
                      Mail host from -> (Rcpt host from [str])
                      Rcpt host from tos -> (Rcpt host from (str:tos)))
              else (rcpt_bad_addr str, st)             

data_bad_order :: SmtpReply
data_bad_order = Reply 503 (hostname ++ " RCPT required before DATA\r\n")

data_ready :: SmtpReply
data_ready = Reply 354
             (hostname ++ " Start mail input; end with <CRLF>.<CRLF>\r\n")

dataComing :: SmtpState -> UpdatedSmtpState
dataComing (Rcpt name from tos) = (data_ready, DataReady name from tos)
dataComing state = (data_bad_order, state)

dataReceived :: SmtpState -> [String] -> UpdatedSmtpState
dataReceived (DataReady name from tos) strs =
    (Reply 250 (hostname ++ " Data recieved\r\n"),
     DataIn  name from tos strs)

rset :: SmtpState -> UpdatedSmtpState
rset Init = (Reply 250 (hostname ++ " State reset.\r\n"), Init)
rset state = (Reply 250 (hostname ++ " State reset.\r\n"), Helo (name state))

noop :: SmtpState -> UpdatedSmtpState
noop = (,) (Reply 250 (hostname ++ " I'm still here.\r\n"))

quit :: SmtpState -> UpdatedSmtpState
quit = (,) (Reply 221 (hostname ++ " It's been a pleasure doing business.\r\n"))

unknown :: SmtpState -> UpdatedSmtpState
unknown = (,) (Reply 504
               (hostname ++ " Command currently not implemented.\r\n"))

is_local_addr :: String -> Bool
is_local_addr str = case (parse getUserDomain "" str) of 
                      Left err -> False
                      Right (usr, dom) -> is_domain dom && is_local_usr usr

is_domain :: String -> Bool
is_domain str = (str == hostname)

is_local_usr :: String -> Bool
is_local_usr str = elem str local_users

getUserDomain :: Parser (String, String)
getUserDomain = do char '<'
                   path <- option "" (try (do ret <- adl
                                              char ':'
                                              return $ ret ++ ":"))
                   user <- localpart
                   char '@'
                   dom <- domain
                   char '>'
                   return $ (user, dom)