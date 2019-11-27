{-# LANGUAGE TupleSections #-}

module Lib where

import           Control.Monad
import           Data.Functor
import           Data.List
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

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

data Definition = 
    DataDefinition {
        name     :: String,
        args     :: [String],
        variants :: [(String, Constructor)]
    } |
    FunctionDefinition {
        name           :: String,
        typeDefinition :: TypeDefinition
    }

instance Show Definition where
    show DataDefinition { name = name, args = args, variants = variants } = 
        "data " ++ name ++ args'
            ++ " = " ++ variants' where
                args' = concat [" " ++ x | x <- args]
                variants' = " | " `intercalate` [x ++ " " ++ show y | (x, y) <- variants]
    show FunctionDefinition { name = name, typeDefinition = typeDefinition } =
        name ++ " : " ++ (show typeDefinition)

data Document =  Document [Definition]

instance Show Document where
    show (Document definitions) = intercalate "\n" (map show definitions)


type Parser = Parsec Void String

lineComment :: Parser ()
lineComment = L.skipLineComment "--"

blockComment :: Parser ()
blockComment = L.skipBlockComment "{-" "-}"

scn :: Parser ()
scn = L.space space1 lineComment blockComment

sc :: Parser ()
sc = L.space (try x <|> try indent) lineComment blockComment where
    space = char ' ' <|> char '\t'
    indent = void $ newline >> some space
    x = void $ (some space >> optional indent)

symbol :: String -> Parser String
symbol = L.symbol sc

identifier :: Parser String
identifier = L.lexeme sc $ some letterChar 

paren :: Parser a -> Parser a
paren a = between (symbol "(" ) (symbol ")") a

variable :: Parser TypeDefinition
variable = identifier <&> Variable

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

dataDefinition :: Parser Definition
dataDefinition = do 
    symbol "data"
    name <- identifier
    args <- many identifier
    symbol "="
    variants <- sepBy1 (do
        name <- identifier
        -- FIXME: why not working order swapped? 
        y <- (try $ record)
            <|> simple
        return $ (name, y))
        $ symbol "|"
    return DataDefinition {name = name, args = args, variants = variants} where
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
            
functionDefinition :: Parser Definition
functionDefinition = do
    name <- identifier
    symbol ":"
    typeDefinition'' <- typeDefinition'
    return $ FunctionDefinition {name = name, typeDefinition = typeDefinition''}

document :: Parser Document
document = do
    x <- manyTill ((try dataDefinition <|> functionDefinition) <* scn) eof
    return $ Document x
