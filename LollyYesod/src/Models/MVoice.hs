{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Models.MVoice
    ( MVoice(..)
    , fID
    , fLANGID
    , fVOICETYPEID
    , fVOICELANG
    , fVOICENAME
    , getDataByLang
    ) where

import Control.Lens
import Data.Aeson
import Data.Default
import Data.Text (Text)
import GHC.Generics (Generic)
import Helpers
import Network.HTTP.Req

data MVoice = MVoice
    { _fID :: Int
    , _fLANGID :: Int
    , _fVOICETYPEID :: Int
    , _fVOICELANG :: Maybe Text
    , _fVOICENAME :: Text
    } deriving (Show, Generic, Default)
makeLenses ''MVoice

instance ToJSON MVoice where
    toJSON = genericToJSON customOptionsLolly

instance FromJSON MVoice where
    parseJSON = genericParseJSON customOptionsLolly

newtype MVoices = MVoices{_frecords :: [MVoice]} deriving (Show, Generic)

instance FromJSON MVoices where
    parseJSON = genericParseJSON customOptionsLolly

getDataByLang :: Int -> IO [MVoice]
getDataByLang langid = runReq def $ do
    v <- req GET (urlLolly /: "VVOICES") NoReqBody jsonResponse $
        "filter" =: ("LANGID,eq," ++ show langid)
    return $ _frecords (responseBody v)
