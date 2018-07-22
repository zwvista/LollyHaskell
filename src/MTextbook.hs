{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module MTextbook
    ( MTextbook
    , MTextbooks
    , getTextbooks
    ) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Req
import Data.Default.Class
import Helpers

data MTextbook = MTextbook
    { fID :: Int
    , fLANGID :: Int
    , fTEXTBOOKNAME :: Text
    , fUNITS :: Int
    , fPARTS :: Text
    } deriving (Show, Generic)

data MTextbooks = MTextbooks { fTEXTBOOKS :: [MTextbook] } deriving (Show, Generic)

customOptions = aesonDrop 1 match where
    match "TEXTBOOKNAME" = "NAME"
    match n = n

instance ToJSON MTextbook where
    toJSON = genericToJSON customOptions

instance FromJSON MTextbook where
    parseJSON = genericParseJSON customOptions

instance FromJSON MTextbooks where
    parseJSON = genericParseJSON customOptions

getTextbooks :: Int -> IO [MTextbook]
getTextbooks langid = runReq def $ do
    v <- req GET (urlLolly /: "TEXTBOOKS") NoReqBody jsonResponse $
        "transform" =: (1 :: Int) <>
        "filter" =: ("LANGID,eq," ++ show langid)
    return $ fTEXTBOOKS (responseBody v :: MTextbooks)
