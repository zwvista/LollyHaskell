{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Models.MTextbook
    ( MTextbook
    , fID
    , fLANGID
    , fTEXTBOOKNAME
    , fUNITS
    , fPARTS
    , getTextbooksByLang
    ) where

import Control.Lens
import Data.Aeson
import Data.Default.Class
import Data.Text (Text)
import GHC.Generics
import Helpers
import Network.HTTP.Req

data MTextbook = MTextbook
    { _fID :: Int
    , _fLANGID :: Int
    , _fTEXTBOOKNAME :: Text
    , _fUNITS :: Int
    , _fPARTS :: Text
    } deriving (Show, Generic)
makeLenses ''MTextbook

data MTextbooks = MTextbooks { _fTEXTBOOKS :: [MTextbook] } deriving (Show, Generic)

customOptions = aesonDrop 2 match where
    match "TEXTBOOKNAME" = "NAME"
    match n = n

instance ToJSON MTextbook where
    toJSON = genericToJSON customOptions

instance FromJSON MTextbook where
    parseJSON = genericParseJSON customOptions

instance FromJSON MTextbooks where
    parseJSON = genericParseJSON customOptions

getTextbooksByLang :: Int -> IO [MTextbook]
getTextbooksByLang langid = runReq def $ do
    v <- req GET (urlLolly /: "TEXTBOOKS") NoReqBody jsonResponse $
        "transform" =: (1 :: Int) <>
        "filter" =: ("LANGID,eq," ++ show langid)
    return $ _fTEXTBOOKS (responseBody v :: MTextbooks)
