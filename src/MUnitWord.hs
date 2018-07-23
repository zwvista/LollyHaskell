{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module MUnitWord
    ( MUnitWord
    , MUnitWords
    , getUnitWordsByTextbook
    , updateUnitWord
    , createUnitWord
    , deleteUnitWord
    ) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Req
import Data.Default.Class
import Text.Printf
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

fWORDNOTE :: MUnitWord -> Text
fWORDNOTE w = fWORD w <> match (fNOTE w) where
    match Nothing = ""
    match (Just "") = ""
    match (Just a) = "(" <> a <> ")"

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
        "filter[]" =: (printf "TEXTBOOKID,eq,%d" textbookid :: String) <>
        "filter[]" =: (printf "UNITPART,bt,%d,%d" unitPartFrom unitPartTo :: String) <>
        "order[]" =: ("UNITPART" :: String) <>
        "order[]" =: ("SEQNUM" :: String)
    return $ fVUNITWORDS (responseBody v :: MUnitWords)

updateUnitWord :: Int -> MUnitWord -> IO (Maybe String)
updateUnitWord id item = runReq def $ do
    v <- req PUT (urlLolly /: "UNITWORDS" /~ id) (ReqBodyJson item) jsonResponse mempty
    return (responseBody v :: Maybe String)

createUnitWord :: MUnitWord -> IO (Maybe String)
createUnitWord item = runReq def $ do
    v <- req POST (urlLolly /: "UNITWORDS") (ReqBodyJson item) jsonResponse mempty
    return (responseBody v :: Maybe String)

deleteUnitWord :: Int -> IO (Maybe String)
deleteUnitWord id = runReq def $ do
    v <- req DELETE (urlLolly /: "UNITWORDS" /~ id) NoReqBody jsonResponse mempty
    return (responseBody v :: Maybe String)
