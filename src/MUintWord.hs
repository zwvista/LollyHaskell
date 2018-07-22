{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module MUnitWord
    ( MUnitWord
    , MUnitWords
    , getUnitWords
    ) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Req
import Data.Default.Class
import Helpers

data MUnitWord = MUnitWord
    { fID :: Int
    , fLANGID :: Int
    , fTEXTBOOKID :: Int
    , fUNIT :: Int
    , fPART :: Int
    , fSEQNUM :: Int
    , fWORD :: Text
    , fNOTE :: Maybe Text
    } deriving (Show, Generic)

getUNITPART :: MUnitWord -> String
getUNITPART w =

data MUnitWords = MUnitWords { fVUNITWORDS :: [MUnitWord] } deriving (Show, Generic)

instance ToJSON MUnitWord where
    toJSON = genericToJSON customOptionsLolly

instance FromJSON MUnitWord where
    parseJSON = genericParseJSON customOptionsLolly

instance FromJSON MUnitWords where
    parseJSON = genericParseJSON customOptionsLolly

getUnitWordsByTextbook :: Int -> Int -> Int -> IO [MUnitWord]
getUnitWordsByTextbook textbookid unitPartFrom unitPartTo = runReq def $ do
    v <- req GET (urlLolly /: "VUNITWORDS") NoReqBody jsonResponse $
        "transform" =: (1 :: Int) <>
        "filter[]" =: ("LANGID,eq," ++ show langid) <>
        "filter[]" =: ("LANGID,eq," ++ show langid) <>
        "order[]" =: ("UNITPART" :: String) <>
        "order[]" =: ("SEQNUM" :: String)
    return $ fVUNITWORDS (responseBody v :: MUnitWords)
