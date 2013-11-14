module Command where
import Char
import Monad
import Text.ParserCombinators.Parsec hiding (anyChar, anyToken)

data SmtpCmd = Helo String
             | Mail String
             | Rcpt String
             | Data
             | Rset
             | Send String
             | Soml String
             | Saml String
             | Vrfy String
             | Expn String
             | Help (Maybe String)
             | Noop
             | Quit
             | Turn

instance Show SmtpCmd where
    show (Helo dmn) = "HELO " ++ dmn
    show (Mail addr) = "MAIL FROM:" ++ addr
    show (Rcpt addr) = "RCPT TO:" ++ addr
    show Data = "DATA"
    show Rset = "RSET"
    show (Send addr) = "SEND FROM:" ++ addr
    show (Soml addr) = "SOML FROM:" ++ addr
    show (Saml addr) = "SAML FROM:" ++ addr
    show (Vrfy addr) = "VRFY " ++ addr
    show (Expn addr) = "EXPN " ++ addr
    show (Help Nothing) = "HELP"
    show (Help (Just str)) = "HELP " ++ str
    show Noop = "NOOP"
    show Quit = "QUIT"
    show Turn = "Turn"

caseString :: String -> Parser String
caseString str = do inStr <- count (length str) anyToken
                    if (map toUpper str) == (map toUpper inStr)
                      then return inStr
                      else unexpected inStr

parseSmtpCmd :: Parser SmtpCmd
parseSmtpCmd = try parseHelo <|>
               try parseMail <|>
               try parseRcpt <|>
               try parseData <|>
               try parseRset <|>
               try parseSend <|>
               try parseSoml <|>
               try parseSaml <|>
               try parseVrfy <|>
               try parseExpn <|>
               try parseHelp <|>
               try parseNoop <|>
               try parseQuit <|>
               try parseTurn

-- These parsers implement the BNF of SMTP commands

parseHelo :: Parser SmtpCmd
parseHelo = do caseString "HELO"
               sp
               name <- domain
               crlf
               return $ Helo name

parseMail :: Parser SmtpCmd
parseMail = do caseString "MAIL"
               sp
               caseString "FROM:"
               addr <- path
               crlf
               return $ Mail addr

parseRcpt :: Parser SmtpCmd
parseRcpt = do caseString "RCPT"
               sp
               caseString "TO:"
               addr <- path
               crlf
               return $ Rcpt addr

parseData :: Parser SmtpCmd
parseData = caseString "DATA" >> crlf >> return Data

parseRset :: Parser SmtpCmd
parseRset = caseString "RSET" >> crlf >> return Data

parseSend :: Parser SmtpCmd
parseSend = do caseString "SEND"
               sp
               caseString "FROM:"
               addr <- path
               crlf
               return $ Send addr

parseSoml :: Parser SmtpCmd
parseSoml = do caseString "SOML"
               sp
               caseString "FROM:"
               addr <- path
               crlf
               return $ Soml addr

parseSaml :: Parser SmtpCmd
parseSaml = do caseString "SAML"
               sp
               caseString "FROM:"
               addr <- path
               crlf
               return $ Saml addr

parseVrfy :: Parser SmtpCmd
parseVrfy = do caseString "VRFY"
               sp
               ret <- anyString
               crlf
               return $ Vrfy ret

parseExpn :: Parser SmtpCmd
parseExpn = do caseString "EXPN"
               sp
               ret <- anyString
               crlf
               return $ Expn ret

parseHelp :: Parser SmtpCmd
parseHelp = do caseString "HELP"
               ((crlf >> (return $ Help Nothing)) <|>
                do sp
                   str <- anyString
                   crlf
                   return $ Help $ Just str)

parseNoop :: Parser SmtpCmd
parseNoop = caseString "NOOP" >> crlf >> return Noop

parseQuit :: Parser SmtpCmd
parseQuit = caseString "QUIT" >> crlf >> return Quit

parseTurn :: Parser SmtpCmd
parseTurn = caseString "TURN" >> crlf >> return Turn

path :: Parser String                   
path = do char '<'
          dom <- option "" (try (do ret <- adl
                                    char ':'
                                    return $ ret ++ ":"))
          box <- mailbox
          char '>'
          return $ "<" ++ dom ++ box ++ ">"

adl :: Parser String
adl = try (do head <- atdomain
              char ','
              tail <- adl
              return $ head ++ "," ++ tail) <|>
      atdomain

atdomain :: Parser String
atdomain = char '@' >> (liftM ("@"++)) domain

domain :: Parser String
domain = try (do head <- element
                 char '.'
                 tail <- domain
                 return $ head ++ "." ++ tail) <|>
         element

element :: Parser String
element = name <|>
          (char '#' >> (liftM ("#"++)) number) <|>
          (do char '['
              ret <- dotnum
              char ']'
              return $ "[" ++ ret ++ "]")

mailbox :: Parser String
mailbox = do lp <- localpart
             char '@'
             dmn <- domain
             return $ (lp ++ "@" ++ dmn)

localpart :: Parser String
localpart = dotstr <|> quotedstring

name :: Parser String
name = do a <- letter
          b <- ldh
          rest <- many1 ldh
          (if (isAlphaNum (rest !! (length rest - 1)))
           then return ([a, b] ++ rest)
           else fail "names must end in a letter digit")

ldh :: Parser Char
ldh = letter <|> digit <|> char '-'

dotstr :: Parser String
dotstr = try (do hd <- anyString
                 char '.'
                 tl <- dotstr
                 return $ hd ++ "." ++ tl) <|>
         anyString

anyString :: Parser String
anyString = (liftM (foldl (++) [])) $ many1 vChar

quotedstring :: Parser String
quotedstring = do char '"'
                  ret <- qtext
                  char '"'
                  return $ ("\"" ++ ret ++ "\"")

qtext :: Parser String
qtext = try (do char '\\'
                x <- anyToken
                ret <- qtext
                return $ "\\" ++ [x] ++ ret) <|>
        try (do x <- anyNQuote
                ret <- qtext
                return $ "\\" ++ [x] ++ ret) <|>
        try (char '\\' >> anyToken >>= (return . (\c -> "\\"++[c]))) <|>
        (liftM (\c -> [c])) anyNQuote

vChar :: Parser String
vChar = (liftM (\c -> [c])) anyChar <|>
        do char '\\'
           x <- anyToken
           return $ "\\" ++ [x]

dotnum :: Parser String
dotnum = do o1 <- snum
            char '.'
            o2 <- snum
            char '.'
            o3 <- snum
            char '.'
            o4 <- snum
            return $ o1 ++ "." ++ o2 ++ "." ++ o3 ++ "." ++ o4

number :: Parser String
number = many1 digit

crlf :: Parser Char
crlf = cr >> lf >> return '\n'

cr :: Parser Char
cr = char $ chr 13

lf :: Parser Char
lf = char $ chr 10

sp :: Parser Char
sp = char ' '

snum :: Parser String
snum = (try $ count 3 digit) <|>
       (try $ count 2 digit) <|>
       count 1 digit

anyChar :: Parser Char
anyChar = notFollowedBy (special <|> sp) >> anyToken

anyNQuote :: Parser Char
anyNQuote = notFollowedBy (cr <|> lf <|> char '"' <|> char '\\') >> anyToken

anyToken :: Parser Char
anyToken = satisfy isAscii

special :: Parser Char
special = oneOf "<>()[]\\.,;:@\"" <|>
          satisfy (\c -> (ord c <= 31) || (ord c == 127))