{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE TupleSections     #-}

module Parser where

import           Control.Monad
import           Data.Char
import           Data.Functor
import           Data.List
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import           AST

type Parser = Parsec Void String

lineComment :: Parser ()
lineComment = void $ hidden (char '#') >> takeWhileP Nothing (\x -> x /= '\n')

blockComment :: Parser ()
blockComment = void empty

scn :: Parser ()
scn = L.space space1 lineComment blockComment

-- single line string literal
stringLiteral' :: Parser () -> Parser String
stringLiteral' sc' = do
    option () sc'
    char '"'
    x <- manyTill L.charLiteral (char '"')
    -- forbid multiline
    if elem '\n' x
        then empty
        else return x 

untilSpace :: Parser String
untilSpace = L.lexeme scn $ takeWhile1P Nothing (\x -> notElem x " \t\n")

symbol' :: Parser () -> String -> Parser ()
symbol' x y = label y $ do
    option () x
    (forM_ y char)

identifier' :: Parser () -> Parser String
identifier' sc' = option () sc' >> (sepBy1 x $ char '.') <&> intercalate "." where
    x = do
        first <- satisfy isAlpha
        rest <- takeWhileP Nothing (\x -> isAlpha x || isDigit x || elem x "'")
        return $ first:rest

term' :: Parser () -> Parser Term
term' sc' = do
    let symbol = symbol' sc'
    let identifier = identifier' sc'
    let arrow x = symbol "->" >> term' sc' <&> Arrow x
    let parenthesized x = between (symbol "(" ) (symbol ")") x
    (try $ do
        argName <- symbol "(" >> identifier
        argType <- symbol ":" >> term' sc' <* symbol ")"
        arrow $ NamedArgument argName argType)
        <|> do
            x <- (try $ identifier <&> Variable) <|> (parenthesized (term' sc') <&> Parenthesized)
            choice [
                try $ arrow $ UnnamedArgument x,
                try $ term' sc' <&> Apply x,
                return x]

indentable :: (Parser () -> Parser a) -> Parser a
indentable x = L.lineFold scn x <* scn

topLevel :: (Parser () -> Parser a) -> Parser a
topLevel = L.nonIndented scn . indentable

annotation :: Parser () -> Parser Annotation
annotation sc' = do
    let symbol = symbol' sc'
    let identifier = identifier' sc'
    -- TODO: better logic for escaping
    let stringLiteral = stringLiteral' sc'
    symbol "@"
    x <- identifier
    (try $ symbol "=" >> (try stringLiteral <|> identifier) <&> AssignmentAnnotation x)
        <|> ((return $ SimpleAnnotation x))
        
annotations' :: Parser () -> Parser Annotations
annotations' sc' = many $ try $ annotation sc'

topLevelAnnotations :: Parser Annotations
topLevelAnnotations = many $ try $ indentable annotation

dataDeclaration :: Annotations -> Parser TopLevelDeclaration
dataDeclaration annotations = topLevel $ \sc' -> do
    let symbol = symbol' sc'
    let identifier = identifier' sc'
    -- TODO: bettern error message
    let record = do
            symbol "{"
            fields <- sepBy1 (try $ do
                name <- identifier
                symbol ":"
                typeDefinition <- term' sc'
                return (name, typeDefinition)) (try $ symbol ",")
            symbol "}"
            return $  Record fields
    let simple = do
            args <- many $ try $ term' sc'
            return $ Simple []
    symbol "data"
    name <- identifier
    args <- many $ try identifier
    symbol "="
    variants <- sepBy1 (try $ do
        annotations <- hidden $ option [] $ annotations' sc'
        name <- identifier
        -- FIXME: why not working order swapped?
        y <- try record <|> simple
        return $ (annotations, name, y))
        $ try $ symbol "|"
    return DataDeclaration { name, args, variants, annotations }
        

typeDeclaration :: Annotations -> Parser TopLevelDeclaration
typeDeclaration annotations = topLevel $ \sc' -> do
    let symbol = symbol' sc'
    let identifier = identifier' sc'
    name <- identifier
    symbol ":"
    term'' <- term' sc'
    return $ TypeDeclaration {name = name, typeDefinition = term'', annotations}

importRule :: Parser () -> Parser ImportRule
importRule sc' =
    let symbol = symbol' sc' in
    let identifier = identifier' sc' in
    option Unqualifed $    
    (try identifier <&> Qualified)
        <|> (try $ (between (symbol "{") (symbol "}") $ sepBy1 (
            do
                x <- identifier
                y <- importRule sc'
                return (x, y)) $ symbol ",")
                <&> UnqualifedOnly)

import' :: Annotations -> Parser TopLevelDeclaration
import' annotations = topLevel $ \sc' -> do
    let symbol = symbol' sc'
    let identifier = identifier' sc'
    let stringLiteral = stringLiteral' sc'
    symbol "import"
    url <- stringLiteral
    rule <- try $ importRule sc'
    return $ Import { url, rule, annotations }

source :: Parser Source
source =
    scn
    >> many
        (do
            x <- hidden $ option [] topLevelAnnotations
            choice $ [import' x, dataDeclaration x, typeDeclaration x
                ])
    <* hidden eof
    <&> Source
