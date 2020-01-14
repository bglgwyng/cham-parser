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

name' :: Parser String
name' = 
    let alphanumeric_ x = isAlpha x || isDigit x || x == '_' in
    (try $ do
        first <- satisfy isAlpha
        rest <- takeWhileP Nothing alphanumeric_
        return $ first:rest)
    <|> do
        first <- char '_'
        rest <- takeWhile1P Nothing alphanumeric_
        return $ first:rest

implicitArguments' :: Parser () -> Parser String
implicitArguments' sc' = option () sc' >> char '\'' >> name'

newIdentifier' :: Parser () -> Parser String
newIdentifier' sc' = option () sc' >> name'

identifier' :: Parser () -> Parser String
identifier' sc' = option () sc' >> (sepBy1 name' $ char '.') <&> intercalate "."

arrow' :: Parser () -> Argument -> Parser Term
arrow' sc' x = symbol "->" >> term' sc' <&> Arrow x where
    symbol = symbol' sc'

-- Term that is not Apply and doesn't start with application like `a b -> c`
term'' :: Parser () -> Parser Term
term'' sc' =
    (try $ identifier <&> Variable)
    <|> (try namedArgument)
    <|> (try $ parenthesized $ term' sc')
    <|> (parenthesized $ sepBy1 (term' sc') (symbol ",") <&> Tuple) where
    symbol = symbol' sc'
    newIdentifier = newIdentifier' sc'
    identifier = identifier' sc'
    parenthesized x = between (symbol "(" ) (symbol ")") x
    arrow = arrow' sc'
    namedArgument = do
        symbol "("
        argName <- newIdentifier
        argType <- symbol ":" >> term' sc'
        symbol ")"
        arrow $ Named argName argType

term' :: Parser () -> Parser Term
term' sc' = do
        x <- (some $ try $ term'' sc') <&> foldl1 Apply
        (try $ arrow $ Unnamed x)
            <|> return x where
    symbol = symbol' sc'
    identifier = newIdentifier' sc'
    arrow = arrow' sc'

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
    (try $ symbol "=" >> (try stringLiteral <|> identifier) <&> Assignment x)
        <|> (return $ Simple x)

annotations' :: Parser () -> Parser Annotations
annotations' sc' = many $ try $ annotation sc'

topLevelAnnotations :: Parser Annotations
topLevelAnnotations = many $ try $ indentable annotation

dataDeclaration :: Annotations -> Parser TopLevelDeclaration
dataDeclaration annotations = topLevel $ \sc' -> do
    let symbol = symbol' sc'
    let newIdentifier = newIdentifier' sc'
    -- TODO: bettern error message
    let record = do
            symbol "{"
            fields <- sepBy1 (try $ do
                name <- newIdentifier
                symbol ":"
                typeDefinition <- term' sc'
                return (name, typeDefinition)) (try $ symbol ",")
            symbol "}"
            return $  Record fields
    let simple = do
            args <- many $ try $ term'' sc'
            return $ Constructor args
    symbol "data"
    name <- newIdentifier
    arguments <- many $ try newIdentifier
    symbol "="
    variants <- sepBy1 (try $ do
        annotations <- hidden $ option [] $ annotations' sc'
        name <- newIdentifier
        -- FIXME: why not working order swapped?
        y <- try record <|> simple
        return $ (annotations, name, y))
        $ try $ symbol "|"
    return DataDeclaration { name, arguments, variants, annotations }


typeDeclaration :: Annotations -> Parser TopLevelDeclaration
typeDeclaration annotations = topLevel $ \sc' -> do
    let symbol = symbol' sc'
    let newIdentifier = newIdentifier' sc'
    name <- newIdentifier
    symbol ":"
    term'' <- term' sc'
    return $ TypeDeclaration { name = name, typeDefinition = term'', annotations }

importRule :: Parser () -> Parser ImportRule
importRule sc' =
    let symbol = symbol' sc' in
    let newIdentifier = newIdentifier' sc' in
    let identifier = identifier' sc' in
    option Unqualified $
    (try newIdentifier <&> Qualified)
    <|> (try $ (between (symbol "{") (symbol "}") $ sepBy1 (
        do
            x <- identifier
            y <- importRule sc'
            return (x, y)) $ try $ symbol ",")
            <&> UnqualifiedOnly)

import' :: Annotations -> Parser TopLevelDeclaration
import' annotations = topLevel $ \sc' -> do
    let symbol = symbol' sc'
    let stringLiteral = stringLiteral' sc'
    symbol "import"
    url <- stringLiteral
    rule <- try $ importRule sc'
    return $ Import { url, rule, annotations }

source :: Parser Source
source =
    (many $ do
        x <- hidden $ option [] topLevelAnnotations
        (try $ import' x)
            <|> (try $ dataDeclaration x)
            <|> typeDeclaration x)
    <* hidden eof
    <&> Source
