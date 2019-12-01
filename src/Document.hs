{-# LANGUAGE TupleSections, NamedFieldPuns, FlexibleInstances #-}

module Document where

import           Control.Monad
import           Data.Functor
import           Data.List
import           Data.Char
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

data Block =
    TaggedBlock { tag :: String, header :: String, body :: [String] } |
    UntaggedBlock { body :: [String] }

instance Show Block where
    show TaggedBlock { tag, header, body } = 
        intercalate "\n" $ ["#[" ++ tag ++ "] " ++ header]
            ++ (("# " ++) <$> body) 
            -- where
            -- parameter (Just key, value) = key ++ "| = |" ++ value
            -- parameter (Nothing, value) = value
    show UntaggedBlock { body } = intercalate "\n" $ ("# " ++) <$> body
    
symbol :: String -> Parser String
symbol = L.symbol sc

sc :: Parser ()
sc = L.space (void $ some space) empty empty where
    space = char ' ' <|> char '\t'
 
stringLiteral :: Parser String
stringLiteral = char '"' >> manyTill L.charLiteral (char '"')

identifier :: Parser String
identifier = 
    (try stringLiteral) 
    <|> (try $ takeWhile1P Nothing (\x -> notElem x " \t=\n"))
    <|> takeWhile1P Nothing (\x -> notElem x "\n")

header' :: Parser (String, String)
header' = do
    spaces
    char '#' >> char '['
    tag <- takeWhile1P Nothing (\x -> notElem x "]\n")
    char ']'
    spaces
    header <- takeWhileP Nothing (\x -> notElem x "\n")
    return (tag, dropWhileEnd isSpace header)


body' = sepEndBy1 (try $ do
    spaces
    char '#'
    (try $ char ' ' >> nonEmptyLine)
        <|> (char '#' >> emptyLine >> return "")) eol
    
            
taggedBlock :: Parser Block
taggedBlock = do
    (tag, header) <- header'
    body <- option [] (eol >> body') 
    return $ TaggedBlock { tag, header, body }

untaggedBlock = do
    body <- body'
    return $ UntaggedBlock { body }

inlineBlock = do
    body <- spaces >> char '#' >> nonEmptyLine
    option () (void eol)
    return $ UntaggedBlock { body = [body] }

nonEol :: Parser String
nonEol = takeWhileP Nothing (\x -> notElem x "\n")

emptyComment :: Parser ()
emptyComment = spaces >> char '#' >> emptyLine

spaces = void $ takeWhileP Nothing (\x -> elem x " \t")
nonSpaces = spaces >> takeWhile1P Nothing (\x -> notElem x " \t\n")


untilEol :: Parser String
untilEol = takeWhileP Nothing (\x -> notElem x "\n")

emptyLine :: Parser ()
emptyLine = do
    x <- untilEol
    if dropWhile isSpace x == ""
        then return ()
        else empty

nonEmptyLine :: Parser String
nonEmptyLine = do
    x <- untilEol
    if dropWhile isSpace x == ""
        then empty
        else return x
    
skipEmptyComments = void $ sepEndBy (try emptyComment) eol

type Document = [Block]
instance {-# Overlapping #-} Show Document where
    show x = concat $ ((++ ";\n") . show) <$> x

document = do
    x <- some $ skipEmptyComments >> (try taggedBlock <|> try untaggedBlock <|> inlineBlock)
    option () skipEmptyComments
    return $ x