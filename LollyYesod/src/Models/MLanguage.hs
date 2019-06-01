{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Models.MLanguage
    ( MLanguage
    , fID
    , fLANGNAME
    , getData
    ) where

import Control.Lens
import Data.Aeson
import Data.Default
import Data.Text (Text)
import GHC.Generics
import Helpers
import Network.HTTP.Req

data MLanguage = MLanguage
    { _fID :: Int
    , _fLANGNAME :: Text
    } deriving (Show, Generic, Default)
makeLenses ''MLanguage

newtype MLanguages = MLanguages{_fLANGUAGES :: [MLanguage]} deriving (Show, Generic)

customOptions :: Options
customOptions = aesonDrop 2 $ \case
    "LANGNAME" -> "NAME"
    n -> n

instance ToJSON MLanguage where
    toJSON = genericToJSON customOptions

instance FromJSON MLanguage where
    parseJSON = genericParseJSON customOptions

instance FromJSON MLanguages where
    parseJSON = genericParseJSON customOptions

getData :: IO [MLanguage]
getData = runReq def $ do
    v <- req GET (urlLolly /: "LANGUAGES") NoReqBody jsonResponse $
        "transform" =: (1 :: Int) <>
        "filter" =: ("ID,neq,0" :: String)
    return $ _fLANGUAGES (responseBody v :: MLanguages)
