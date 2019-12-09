{-# LANGUAGE TupleSections, NamedFieldPuns, FlexibleInstances #-}

module Lib (root) where 

import           Control.Monad
import           Data.Functor
import           Data.List
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data Annotation = SimpleAnnotation String | AssignmentAnnotation String String

type Annotations = [Annotation]

instance Show Annotation where
    show (SimpleAnnotation x) = "@" ++ x
    show (AssignmentAnnotation x y) = "@" ++ x ++ " = " ++ y

instance {-# Overlapping #-} Show Annotations where
    show xs = concat ((++ "\n") . show <$> xs)

data FunctionArgument =
    Named String TypeDefinition |
    Unnamed TypeDefinition

instance Show FunctionArgument where
    show (Named x y) = "(" ++ x ++ " : " ++ show y ++ ")"
    show (Unnamed y) = show y

data TypeDefinition =
    Arrow FunctionArgument TypeDefinition |
    Apply TypeDefinition TypeDefinition |
    Paren TypeDefinition |
    Variable String

instance Show TypeDefinition where
    show x = case x of
        Arrow y z  -> show y ++ " -> " ++ show z
        Apply y z  -> show y ++ " " ++ show z
        Paren y    -> "(" ++ show y ++ ")"
        Variable y -> y

data Constructor =
    Simple [TypeDefinition] |
    Record [(String, TypeDefinition)]

instance Show Constructor where
    show (Simple as) = intercalate " " (map show as)
    show (Record fields) = "{ " ++ intercalate ", " [x ++ " : " ++ show y | (x, y) <- fields] ++ " }"
    
-- TODO: Better name
data ImportRule = Unqualifed | UnqualifedOnly [(String, ImportRule)] | Qualified String

instance Show ImportRule where
    show Unqualifed = "";
    show (UnqualifedOnly xs) = " { " ++ intercalate ", " [x ++ show y | (x, y) <- xs] ++ " }"
    show (Qualified xs) = " " ++ xs

data TopLevelStatement =
    DataDefinition {
        name     :: String,
        args     :: [String],
        variants :: [(Annotations, String, Constructor)],
        annotations :: Annotations
    } |
    FunctionDefinition {
        name           :: String,
        typeDefinition :: TypeDefinition,
        annotations :: Annotations
    } | 
    Import {
        url :: String,
        rule :: ImportRule,
        annotations :: Annotations
    }

instance Show TopLevelStatement where
    show DataDefinition { name, args, variants, annotations } =
        show annotations
        ++ "data " ++ name ++ args'
        ++ " = " ++ variants' where
            args' = concat [" " ++ x | x <- args]
            variants' = " | " `intercalate` [show x ++ y ++ " " ++ show z | (x, y, z) <- variants]
    show FunctionDefinition { name, typeDefinition, annotations } =
        show annotations
        ++ name ++ " : " ++ (show typeDefinition)
    show Import { annotations, url, rule } =
        show annotations
        ++ "import " ++ show url ++ show rule

data Root = Root [TopLevelStatement]

instance Show Root where
    show (Root definitions) = intercalate "\n" (map show definitions)


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
identifier = L.lexeme sc $ some letterChar

paren :: Parser a -> Parser a
paren a = between (symbol "(" ) (symbol ")") a

variable :: Parser TypeDefinition
variable = identifier <&> Variable

annotations' :: Parser Annotations
annotations' = many $ do
    symbol "@"
    x <- identifier
    (try $ symbol "=" >> ((try stringLiteral <* scn) <|> untilSpace) <&> AssignmentAnnotation x)
        <|> (scn >> (return $ SimpleAnnotation x))

arrow :: FunctionArgument -> Parser TypeDefinition
arrow x = symbol "->" >> typeDefinition' <&> Arrow x

typeDefinition' :: Parser TypeDefinition
typeDefinition' =
    (try $ do
        argName <- symbol "(" >> identifier
        argType <- symbol ":" >> typeDefinition' <* symbol ")"
        arrow $ Named argName argType)
        <|> do
            x <- variable <|> ((paren typeDefinition') <&> Paren)
            choice [
                arrow $ Unnamed x,
                typeDefinition' <&> Apply x,
                return x]

dataDefinition :: Annotations -> Parser TopLevelStatement
dataDefinition annotations = do
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
    return DataDefinition { name, args, variants, annotations } where
        record = do
            symbol "{"
            fields <- sepBy (do
                name <- identifier
                symbol ":"
                typeDefinition <- typeDefinition'
                return (name, typeDefinition)) (symbol ",")
            symbol "}"
            return $  Record fields
        simple = do
            args <- many typeDefinition'
            return $ Simple args

functionDefinition :: Annotations -> Parser TopLevelStatement
functionDefinition annotations = do
    name <- identifier
    symbol ":"
    typeDefinition'' <- typeDefinition'
    return $ FunctionDefinition {name = name, typeDefinition = typeDefinition'', annotations}

importRule :: Parser ImportRule
importRule = option Unqualifed 
    ((try identifier <&> Qualified)
        <|> ((between (symbol "{") (symbol "}") $ sepBy1 (
            do
                x <- identifier
                y <- importRule
                return (x, y)) $ symbol ",")
                <&> UnqualifedOnly))

import' :: Annotations -> Parser TopLevelStatement
import' annotations = do
    symbol "import"
    url <- stringLiteral
    rule <- importRule
    return $ Import { url, rule, annotations }

root :: Parser Root
root =
    scn 
    >> manyTill
        (do
            x <- option [] (annotations')
            choice [import' x, dataDefinition x, functionDefinition x]
            <* scn) 
        eof
    <&> Root
