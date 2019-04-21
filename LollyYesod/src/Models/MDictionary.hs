{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Models.MDictionary
    ( MDictionary
    , fID
    , fDICTID
    , fLANGIDFROM
    , fDICTTYPENAME
    , fDICTNAME
    , fURL
    , fCHCONV
    , fTRANSFORM
    , fWAIT
    , fTEMPLATE
    ) where

import Control.Lens
import Data.Aeson
import Data.Default.Class
import Data.Text (Text)
import GHC.Generics
import Helpers
import Network.HTTP.Req

data MDictionary = MDictionary
    { _fID :: Int
    , _fDICTID :: Int
    , _fLANGIDFROM :: Int
    , _fDICTTYPENAME :: Text
    , _fDICTNAME :: Text
    , _fURL :: Maybe Text
    , _fCHCONV :: Maybe Text
    , _fTRANSFORM :: Maybe Text
    , _fWAIT :: Maybe Int
    , _fTEMPLATE :: Maybe Text
    } deriving (Show, Generic)
makeLenses ''MDictionary

instance ToJSON MDictionary where
    toJSON = genericToJSON customOptionsLolly

instance FromJSON MDictionary where
    parseJSON = genericParseJSON customOptionsLolly