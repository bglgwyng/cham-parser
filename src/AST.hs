{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}

module AST where

import           Data.List

data Annotation =
    SimpleAnnotation String |
    AssignmentAnnotation String String

instance Show Annotation where
    show (SimpleAnnotation x)       = "@" ++ x
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
    Parenthesized Term |
    Variable String

instance Show Term where
    show x = case x of
        Arrow y z  -> show y ++ " -> " ++ show z
        Apply y z  -> show y ++ " " ++ show z
        Parenthesized y    -> "(" ++ show y ++ ")"
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
        name        :: String,
        args        :: [String],
        variants    :: [(Annotations, String, Constructor)],
        annotations :: Annotations
    } |
    TypeDeclaration {
        name           :: String,
        typeDefinition :: Term,
        annotations    :: Annotations
    } |
    Import {
        url         :: String,
        rule        :: ImportRule,
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
