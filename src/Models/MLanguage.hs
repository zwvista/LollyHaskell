{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Models.MLanguage
    ( MLanguage
    , fID
    , fLANGNAME
    , getLanguages
    ) where

import Control.Lens
import Data.Aeson
import Data.Default.Class
import Data.Text (Text)
import GHC.Generics
import Helpers
import Network.HTTP.Req

data MLanguage = MLanguage
    { _fID :: Int
    , _fLANGNAME :: Text
    } deriving (Show, Generic)
makeLenses ''MLanguage

data MLanguages = MLanguages { _fLANGUAGES :: [MLanguage] } deriving (Show, Generic)

customOptions = aesonDrop 2 match where
    match "LANGNAME" = "NAME"
    match n = n

instance ToJSON MLanguage where
    toJSON = genericToJSON customOptions

instance FromJSON MLanguage where
    parseJSON = genericParseJSON customOptions

instance FromJSON MLanguages where
    parseJSON = genericParseJSON customOptions

getLanguages :: IO [MLanguage]
getLanguages = runReq def $ do
    v <- req GET (urlLolly /: "LANGUAGES") NoReqBody jsonResponse $
        "transform" =: (1 :: Int) <>
        "filter" =: ("ID,neq,0" :: String)
    return $ _fLANGUAGES (responseBody v :: MLanguages)
