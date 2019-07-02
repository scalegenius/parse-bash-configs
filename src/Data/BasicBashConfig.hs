{-# LANGUAGE OverloadedStrings #-}

module Data.BasicBashConfig
( BasicBashConfig(..)
, parseFile
, basic_bash_config_line_parser
, basic_bash_config_file_parser
, read_config_from_file 
)
where

import Prelude hiding (lines)

import Data.ByteString as B
import Data.Attoparsec.ByteString.Char8
import Control.Applicative


import Data.Maybe


-- | Primary Data for Bash Arrays
--
data BasicBashConfig = BasicBashConfig {
    configName  :: String,
    configLines :: [B.ByteString]
} deriving (Show, Eq)

-- |
-- 
parseFile :: ByteString -> Either String [BasicBashConfig]
parseFile b = parseOnly basic_bash_config_file_parser b

read_config_from_file :: String -> IO [BasicBashConfig]
read_config_from_file file = do
        contents <- (B.readFile file)
        return $ read_config_from_bytestring contents

read_config_from_bytestring :: B.ByteString -> [BasicBashConfig]
read_config_from_bytestring fc = select_config $ parseFile fc


select_config :: Either String [BasicBashConfig] -> [BasicBashConfig]
select_config (Right bc) = bc
select_config (Left _) = []




-- these two exist for testing purposes
basic_bash_config_line_parser :: Parser (Maybe BasicBashConfig)
basic_bash_config_line_parser = line

basic_bash_config_file_parser :: Parser [BasicBashConfig]
basic_bash_config_file_parser = file


-- Start Parser Definitions
line :: Parser (Maybe BasicBashConfig)
line = do
          skipMany space
          try (comment >> return Nothing) <|> (item >>= return . Just)

file :: Parser [BasicBashConfig]
file = do
          lines <- many line
          return (catMaybes lines)

--------------------------------------------------------------------------------
-- | Skip all characters up to and including the next EOL.
skipLine :: Parser ()
skipLine = skipWhile (not . isEOL) >> eol

isEOL :: Char -> Bool
isEOL x = x == '\n'

comment :: Parser ()
comment = char8 '#' >> skipLine

eol :: Parser ()
eol =  skipWhile (isEOL)
     <?> "end of line"


skipWhiteSpace :: Parser ()
skipWhiteSpace = skipWhile spaces
        where spaces x = x == ' ' || x == '\t' || (isEOL x)


isCap :: Char -> Bool
isCap x = x >= 'A' && x <= 'Z'


ident :: Parser String
ident = do c <- satisfy isCap
           cs <- many' $ satisfy $ inClass "A-Z_"
           return (c:cs)
      <?> "identifier"


valueItem :: Parser B.ByteString
valueItem = do
        skipWhiteSpace >> char '"'
        v <- takeTill (== '"')
        char '"' >> skipWhiteSpace >> try comment <|> eol
        return v

item :: Parser BasicBashConfig
item = do key <- ident
          string "=("
          skipWhiteSpace
          value <- many $ valueItem
          skipWhiteSpace
          string ")\n"
          return $ BasicBashConfig key value
