{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}

module AST where

import           Data.Aeson
import           Data.Aeson.Encoding (text)
import           Data.List
import           GHC.Generics        hiding (Constructor)

data Annotation =
    Simple String |
    Assignment String String
    deriving Generic

type Annotations = [Annotation]

data Argument =
    Named String Term |
    Unnamed Term
    deriving Generic

data Term =
    Arrow Argument Term |
    Apply Term Term |
    Tuple [Term] |
    Variable String
    deriving Generic

data Constructor =
    Constructor [Term] |
    Record [(String, Term)]
    deriving Generic

-- TODO: Better name
data ImportRule =
    Unqualified |
    UnqualifiedOnly [(String, ImportRule)] |
    Qualified String
    deriving Generic

data TopLevelDeclaration =
    DataDeclaration {
        name        :: String,
        arguments   :: [String],
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

data Source = Source [TopLevelDeclaration] deriving Generic


options :: Options
options = defaultOptions { sumEncoding = TwoElemArray, unwrapUnaryRecords = True, allNullaryToStringTag  = True }

instance ToJSON Annotation where
    toEncoding = genericToEncoding options

instance ToJSON Argument where
    toEncoding = genericToEncoding options

instance ToJSON Term where
    toEncoding = genericToEncoding options

instance ToJSON Constructor where
    toEncoding = genericToEncoding options

instance ToJSON ImportRule where
    -- FIXME: allNullaryToStringTag doesn't work...
    toEncoding Unqualified = text "Unqualified"
    toEncoding x           = genericToEncoding options x

instance ToJSON TopLevelDeclaration where
    toEncoding = genericToEncoding options

instance ToJSON Source where
    toEncoding = genericToEncoding options
