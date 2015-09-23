module Parser (readExpr) where

import LispVal
import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric (readOct, readHex, readInt, readFloat)
import Data.Char (digitToInt)
import Control.Monad.Error
import Control.Monad (liftM)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first : rest
    return $ Atom atom

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail
    
parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
    char '`'
    x <- parseExpr
    return $ List [Atom "quasiquote", x]
     
parseUnQuote :: Parser LispVal
parseUnQuote = do
    char ','
    x <- parseExpr
    return $ List [Atom "unquote", x]

escapedChars :: Parser Char
escapedChars = do
    char '\\'
    c <- oneOf "\\\"nrt"
    return $ case c of
        '\\' -> c
        '"'  -> c
        'n'  -> '\n'
        'r'  -> '\r'
        't'  -> '\t'

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many (escapedChars <|> noneOf "\\\"")
    char '"'
    return $ String x            

parseBool :: Parser LispVal
parseBool = do
    char '#'
    c <- oneOf "tf"
    return . Bool $ if c == 't' then True else False

parseCharacter :: Parser LispVal
parseCharacter = do
    string "#\\"
    v <- try $ string "newline" <|> string "space"
        <|> do
            c <- anyChar
            notFollowedBy alphaNum
            return [c]
    return . Character $ case v of
        "space"   -> ' '
        "newline" -> '\n'
        [c]       -> c
        
parseNumber :: Parser LispVal
parseNumber = parsePlainNumber
    <|> try parseBin
    <|> try parseOct
    <|> try parseDec
    <|> parseHex

parsePlainNumber :: Parser LispVal
parsePlainNumber = do
    x <- many1 digit
    return . Number . read $ x
    
parseBin :: Parser LispVal
parseBin = do
    string "#b"
    x <- many1 (oneOf "01")
    return . Number . fst . head . (readInt 2 (`elem` "01") digitToInt) $ x

parseOct :: Parser LispVal
parseOct = do
    string "#o"
    x <- many1 octDigit
    return . Number . fst . head . readOct $ x

parseHex :: Parser LispVal
parseHex = do
    string "#x"
    x <- many1 hexDigit
    return . Number . fst . head . readHex $ x
    
parseDec :: Parser LispVal
parseDec = do
    string "#d"
    x <- many1 digit
    return . Number . read $ x

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> try parseNumber
        <|> try parseBool
        <|> parseCharacter
        <|> parseQuoted
        <|> do
            open <- oneOf "([{"
            x <- try parseList <|> parseDottedList
            let close = case open of
                    '(' -> ')'
                    '[' -> ']'
                    '{' -> '}'
            char close
            return x

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val
