{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Models.MUnitPhrase
    ( MUnitPhrase
    , fID
    , fLANGID
    , fTEXTBOOKID
    , fUNIT
    , fPART
    , fSEQNUM
    , fPHRASE
    , fTRANSLATION
    , getDataByTextbookUnitPart
    , update
    , create
    , delete
    ) where

import Control.Lens
import Data.Aeson
import Data.Default
import Data.Text (Text)
import GHC.Generics
import Helpers
import Network.HTTP.Req
import Text.Printf

data MUnitPhrase = MUnitPhrase
    { _fID :: Int
    , _fLANGID :: Int
    , _fTEXTBOOKID :: Int
    , _fUNIT :: Int
    , _fPART :: Int
    , _fSEQNUM :: Int
    , _fPHRASE :: Text
    , _fTRANSLATION :: Maybe Text
    } deriving (Show, Generic, Default)
makeLenses ''MUnitPhrase

newtype MUnitPhrases = MUnitPhrases{_fVUNITPHRASES :: [MUnitPhrase]} deriving (Show, Generic)

instance ToJSON MUnitPhrase where
    toJSON = genericToJSON customOptionsLolly

instance FromJSON MUnitPhrase where
    parseJSON = genericParseJSON customOptionsLolly

instance FromJSON MUnitPhrases where
    parseJSON = genericParseJSON customOptionsLolly

getDataByTextbookUnitPart :: Int -> Int -> Int -> IO [MUnitPhrase]
getDataByTextbookUnitPart textbookid unitPartFrom unitPartTo = runReq def $ do
    v <- req GET (urlLolly /: "VUNITPHRASES") NoReqBody jsonResponse $
        "transform" =: (1 :: Int) <>
        "filter[]" =: (printf "TEXTBOOKID,eq,%d" textbookid :: String) <>
        "filter[]" =: (printf "UNITPART,bt,%d,%d" unitPartFrom unitPartTo :: String) <>
        "order[]" =: ("UNITPART" :: String) <>
        "order[]" =: ("SEQNUM" :: String)
    return $ _fVUNITPHRASES (responseBody v :: MUnitPhrases)

update :: Int -> MUnitPhrase -> IO (Maybe String)
update fid item = runReq def $ do
    v <- req PUT (urlLolly /: "UNITPHRASES" /~ fid) (ReqBodyJson item) jsonResponse mempty
    return (responseBody v :: Maybe String)

create :: MUnitPhrase -> IO (Maybe String)
create item = runReq def $ do
    v <- req POST (urlLolly /: "UNITPHRASES") (ReqBodyJson item) jsonResponse mempty
    return (responseBody v :: Maybe String)

delete :: Int -> IO (Maybe String)
delete fid = runReq def $ do
    v <- req DELETE (urlLolly /: "UNITPHRASES" /~ fid) NoReqBody jsonResponse mempty
    return (responseBody v :: Maybe String)
