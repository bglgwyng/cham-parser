{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}

module AST where

import           Data.Aeson
-- import           Data.Vector
import           Data.List
import           GHC.Generics hiding (Constructor)

data Annotation =
    SimpleAnnotation String |
    AssignmentAnnotation String String
    deriving Generic

instance Show Annotation where
    show (SimpleAnnotation x)       = "@" ++ x
    show (AssignmentAnnotation x y) = "@" ++ x ++ " = " ++ y

type Annotations = [Annotation]

instance {-# Overlapping #-} Show Annotations where
    show xs = concat ((++ "\n") . show <$> xs)

data Argument =
    NamedArgument String Term |
    UnnamedArgument Term
    deriving Generic

instance Show Argument where
    show (NamedArgument x y) = "(" ++ x ++ " : " ++ show y ++ ")"
    show (UnnamedArgument y) = show y

data Term =
    Arrow Argument Term |
    Apply Term Term |
    Parenthesized Term |
    Variable String
    deriving Generic

instance Show Term where
    show x = case x of
        Arrow y z       -> show y ++ " -> " ++ show z
        Apply y z       -> show y ++ " " ++ show z
        Parenthesized y -> "(" ++ show y ++ ")"
        Variable y      -> y

data Constructor =
    Simple [Term] |
    Record [(String, Term)]
    deriving Generic

instance Show Constructor where
    show (Simple as) = concat (map ((" " ++) . show) as)
    show (Record fields) = " { " ++ intercalate ", " [x ++ " : " ++ show y | (x, y) <- fields] ++ " }"

-- TODO: Better name
data ImportRule =
    Unqualifed |
    UnqualifedOnly [(String, ImportRule)] |
    Qualified String
    deriving Generic

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
    deriving Generic


instance Show TopLevelDeclaration where
    show DataDeclaration { name, args, variants, annotations } =
        show annotations
        ++ "data " ++ name ++ args'
        ++ " = " ++ variants' where
            args' = concat [" " ++ x | x <- args]
            variants' = " | " `intercalate` [show x ++ y ++ show z | (x, y, z) <- variants]
    show TypeDeclaration { name, typeDefinition, annotations } =
        show annotations
        ++ name ++ " : " ++ (show typeDefinition)
    show Import { annotations, url, rule } =
        show annotations
        ++ "import " ++ show url ++ show rule

data Source = Source [TopLevelDeclaration] deriving Generic

instance Show Source where
    show (Source definitions) = intercalate "\n" (map show definitions)

options :: Options    
options = defaultOptions { sumEncoding = TwoElemArray }

instance ToJSON Annotation where
    toEncoding = genericToEncoding options
    
instance ToJSON Argument where
    toEncoding = genericToEncoding options
        
instance ToJSON Term where
    toEncoding = genericToEncoding options

instance ToJSON Constructor where
    toEncoding = genericToEncoding options

instance ToJSON ImportRule where
    toEncoding = genericToEncoding options

instance ToJSON TopLevelDeclaration where
    toEncoding = genericToEncoding options

instance ToJSON Source where
    toEncoding = genericToEncoding options
