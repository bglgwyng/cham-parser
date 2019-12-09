{-# LANGUAGE TupleSections, NamedFieldPuns, FlexibleInstances #-}

module Lib (source) where 

import           Control.Monad
import           Data.Functor
import           Data.Char
import           Data.List
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data Annotation = SimpleAnnotation String | AssignmentAnnotation String String

instance Show Annotation where
    show (SimpleAnnotation x) = "@" ++ x
    show (AssignmentAnnotation x y) = "@" ++ x ++ " = " ++ y

type Annotations = [Annotation]

instance {-# Overlapping #-} Show Annotations where
    show xs = concat ((++ "\n") . show <$> xs)

data Argument =
    NamedArgument String Term |
    UnnamedArgument Term

instance Show Argument where
    show (NamedArgument x y) = "(" ++ x ++ " : " ++ show y ++ ")"
    show (UnnamedArgument y) = show y

data Term =
    Arrow Argument Term |
    Apply Term Term |
    Paren Term |
    Variable String

instance Show Term where
    show x = case x of
        Arrow y z  -> show y ++ " -> " ++ show z
        Apply y z  -> show y ++ " " ++ show z
        Paren y    -> "(" ++ show y ++ ")"
        Variable y -> y

data Constructor =
    Simple [Term] |
    Record [(String, Term)]

instance Show Constructor where
    show (Simple as) = intercalate " " (map show as)
    show (Record fields) = "{ " ++ intercalate ", " [x ++ " : " ++ show y | (x, y) <- fields] ++ " }"
    
-- TODO: Better name
data ImportRule = Unqualifed | UnqualifedOnly [(String, ImportRule)] | Qualified String

instance Show ImportRule where
    show Unqualifed = "";
    show (UnqualifedOnly xs) = " { " ++ intercalate ", " [x ++ show y | (x, y) <- xs] ++ " }"
    show (Qualified xs) = " " ++ xs

data TopLevelDeclaration =
    DataDeclaration {
        name     :: String,
        args     :: [String],
        variants :: [(Annotations, String, Constructor)],
        annotations :: Annotations
    } |
    TypeDeclaration {
        name           :: String,
        typeDefinition :: Term,
        annotations :: Annotations
    } | 
    Import {
        url :: String,
        rule :: ImportRule,
        annotations :: Annotations
    }

instance Show TopLevelDeclaration where
    show DataDeclaration { name, args, variants, annotations } =
        show annotations
        ++ "data " ++ name ++ args'
        ++ " = " ++ variants' where
            args' = concat [" " ++ x | x <- args]
            variants' = " | " `intercalate` [show x ++ y ++ " " ++ show z | (x, y, z) <- variants]
    show TypeDeclaration { name, typeDefinition, annotations } =
        show annotations
        ++ name ++ " : " ++ (show typeDefinition)
    show Import { annotations, url, rule } =
        show annotations
        ++ "import " ++ show url ++ show rule

data Source = Source [TopLevelDeclaration]

instance Show Source where
    show (Source definitions) = intercalate "\n" (map show definitions)


type Parser = Parsec Void String

lineComment :: Parser ()
lineComment = void $ hidden (char '#') >> takeWhileP Nothing (\x -> x /= '\n')

blockComment :: Parser ()
blockComment = void empty

scn :: Parser ()
scn = L.space space1 lineComment blockComment

sc :: Parser ()
sc = L.space (try x <|> try indent) lineComment blockComment where
    space = char ' ' <|> char '\t'
    indent = void $ eol >> some space
    x = void $ (some space >> optional indent)

symbol :: String -> Parser String
symbol = L.symbol sc

stringLiteral :: Parser String
stringLiteral = L.lexeme sc $ char '"' >> manyTill L.charLiteral (char '"')

untilSpace :: Parser String
untilSpace = L.lexeme scn $ takeWhile1P Nothing (\x -> notElem x " \t\n")

identifier :: Parser String
identifier = L.lexeme sc $ (sepBy1 x $ char '.') <&> intercalate "." where
    x = do
        first <- satisfy isAlpha
        rest <- takeWhileP Nothing (\x -> isAlpha x || isDigit x || elem x "'")
        return $ first:rest

parenthesized :: Parser a -> Parser a
parenthesized a = between (symbol "(" ) (symbol ")") a

variable :: Parser Term
variable = identifier <&> Variable

annotations' :: Parser Annotations
annotations' = many $ do
    symbol "@"
    x <- identifier
    (try $ symbol "=" >> ((try stringLiteral <* scn) <|> untilSpace) <&> AssignmentAnnotation x)
        <|> (scn >> (return $ SimpleAnnotation x))

arrow :: Argument -> Parser Term
arrow x = symbol "->" >> term <&> Arrow x

term :: Parser Term
term =
    (try $ do
        argName <- symbol "(" >> identifier
        argType <- symbol ":" >> term <* symbol ")"
        arrow $ NamedArgument argName argType)
        <|> do
            x <- variable <|> ((parenthesized term) <&> Paren)
            choice [
                arrow $ UnnamedArgument x,
                term <&> Apply x,
                return x]

dataDeclaration :: Annotations -> Parser TopLevelDeclaration
dataDeclaration annotations = do
    symbol "data"
    name <- identifier
    args <- many identifier
    symbol "="
    variants <- sepBy1 (do
        annotations <- annotations'
        name <- identifier
        -- FIXME: why not working order swapped?
        y <- (try $ record)
            <|> simple
        return $ (annotations, name, y))
        $ symbol "|"
    return DataDeclaration { name, args, variants, annotations } where
        record = do
            symbol "{"
            fields <- sepBy (do
                name <- identifier
                symbol ":"
                typeDefinition <- term
                return (name, typeDefinition)) (symbol ",")
            symbol "}"
            return $  Record fields
        simple = do
            args <- many term
            return $ Simple args

typeDeclaration :: Annotations -> Parser TopLevelDeclaration
typeDeclaration annotations = do
    name <- identifier
    symbol ":"
    term' <- term
    return $ TypeDeclaration {name = name, typeDefinition = term', annotations}

importRule :: Parser ImportRule
importRule = option Unqualifed 
    ((try identifier <&> Qualified)
        <|> ((between (symbol "{") (symbol "}") $ sepBy1 (
            do
                x <- identifier
                y <- importRule
                return (x, y)) $ symbol ",")
                <&> UnqualifedOnly))

import' :: Annotations -> Parser TopLevelDeclaration
import' annotations = do
    symbol "import"
    url <- stringLiteral
    rule <- importRule
    return $ Import { url, rule, annotations }

source :: Parser Source
source =
    scn 
    >> manyTill
        (do
            x <- option [] (annotations')
            choice [import' x, dataDeclaration x, typeDeclaration x]
            <* scn) 
        eof
    <&> Source
