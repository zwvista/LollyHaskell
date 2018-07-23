{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module MUnitPhrase
    ( MUnitPhrase
    , MUnitPhrases
    , getUnitPhrasesByTextbook
    , updateUnitPhrase
    , createUnitPhrase
    , deleteUnitPhrase
    ) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Req
import Data.Default.Class
import Text.Printf
import Helpers

data MUnitPhrase = MUnitPhrase
    { fID :: Int
    , fLANGID :: Int
    , fTEXTBOOKID :: Int
    , fUNIT :: Int
    , fPART :: Int
    , fSEQNUM :: Int
    , fPHRASE :: Text
    , fTRANSLATION :: Maybe Text
    } deriving (Show, Generic)

data MUnitPhrases = MUnitPhrases { fVUNITPHRASES :: [MUnitPhrase] } deriving (Show, Generic)

instance ToJSON MUnitPhrase where
    toJSON = genericToJSON customOptionsLolly

instance FromJSON MUnitPhrase where
    parseJSON = genericParseJSON customOptionsLolly

instance FromJSON MUnitPhrases where
    parseJSON = genericParseJSON customOptionsLolly

getUnitPhrasesByTextbook :: Int -> Int -> Int -> IO [MUnitPhrase]
getUnitPhrasesByTextbook textbookid unitPartFrom unitPartTo = runReq def $ do
    v <- req GET (urlLolly /: "VUNITPHRASES") NoReqBody jsonResponse $
        "transform" =: (1 :: Int) <>
        "filter[]" =: (printf "TEXTBOOKID,eq,%d" textbookid :: String) <>
        "filter[]" =: (printf "UNITPART,bt,%d,%d" unitPartFrom unitPartTo :: String) <>
        "order[]" =: ("UNITPART" :: String) <>
        "order[]" =: ("SEQNUM" :: String)
    return $ fVUNITPHRASES (responseBody v :: MUnitPhrases)

updateUnitPhrase :: Int -> MUnitPhrase -> IO (Maybe String)
updateUnitPhrase id item = runReq def $ do
    v <- req PUT (urlLolly /: "UNITPHRASES" /~ id) (ReqBodyJson item) jsonResponse mempty
    return (responseBody v :: Maybe String)

createUnitPhrase :: MUnitPhrase -> IO (Maybe String)
createUnitPhrase item = runReq def $ do
    v <- req POST (urlLolly /: "UNITPHRASES") (ReqBodyJson item) jsonResponse mempty
    return (responseBody v :: Maybe String)

deleteUnitPhrase :: Int -> IO (Maybe String)
deleteUnitPhrase id = runReq def $ do
    v <- req DELETE (urlLolly /: "UNITPHRASES" /~ id) NoReqBody jsonResponse mempty
    return (responseBody v :: Maybe String)
