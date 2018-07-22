{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module MLanguage
    ( MLanguage
    , MLanguages
    , getLanguages
    ) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Req
import Data.Default.Class
import Helpers

data MLanguage = MLanguage
    { fID :: Int
    , fLANGNAME :: Text
    } deriving (Show, Generic)

data MLanguages = MLanguages { fLANGUAGES :: [MLanguage] } deriving (Show, Generic)

customOptions = aesonDrop 1 match where
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
    return $ fLANGUAGES (responseBody v :: MLanguages)
