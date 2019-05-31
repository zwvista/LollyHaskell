{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Models.MDictionary
    ( MDictionary
    , fID
    , fDICTID
    , fLANGIDFROM
    , fLANGNAMEFROM
    , fLANGIDTO
    , fLANGNAMETO
    , fSEQNUM
    , fDICTTYPENAME
    , fDICTNAME
    , fURL
    , fCHCONV
    , fAUTOMATION
    , fAUTOJUMP
    , fDICTTABLE
    , fTRANSFORM_WIN
    , fTRANSFORM
    , fWAIT
    , fTEMPLATE
    , fTEMPLATE2
    ) where

import Control.Lens
import Data.Aeson
import Data.Default.Class
import Data.Text (Text)
import GHC.Generics
import Helpers

data MDictionary = MDictionary
    { _fID :: Int
    , _fDICTID :: Int
    , _fLANGIDFROM :: Int
    , _fLANGNAMEFROM :: Text
    , _fLANGIDTO :: Int
    , _fLANGNAMETO :: Text
    , _fSEQNUM :: Int
    , _fDICTTYPENAME :: Text
    , _fDICTNAME :: Text
    , _fURL :: Maybe Text
    , _fCHCONV :: Maybe Text
    , _fAUTOMATION :: Maybe Text
    , _fAUTOJUMP :: Int
    , _fDICTTABLE :: Maybe Text
    , _fTRANSFORM_WIN :: Maybe Text
    , _fTRANSFORM :: Maybe Text
    , _fWAIT :: Maybe Int
    , _fTEMPLATE :: Maybe Text
    , _fTEMPLATE2 :: Maybe Text
    } deriving (Show, Generic, Default)
makeLenses ''MDictionary

instance ToJSON MDictionary where
    toJSON = genericToJSON customOptionsLolly

instance FromJSON MDictionary where
    parseJSON = genericParseJSON customOptionsLolly
