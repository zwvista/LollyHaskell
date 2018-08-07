{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Models.MUnitWord
    ( MUnitWord
    , fID
    , fLANGID
    , fTEXTBOOKID
    , fUNIT
    , fPART
    , fSEQNUM
    , fWORD
    , fNOTE
    , getDataByTextbookUnitPart
    , update
    , create
    , delete
    ) where

import Control.Lens
import Data.Aeson
import Data.Default.Class
import Data.Text (Text)
import GHC.Generics
import Helpers
import Network.HTTP.Req
import Text.Printf

data MUnitWord = MUnitWord
    { _fID :: Int
    , _fLANGID :: Int
    , _fTEXTBOOKID :: Int
    , _fUNIT :: Int
    , _fPART :: Int
    , _fSEQNUM :: Int
    , _fWORD :: Text
    , _fNOTE :: Maybe Text
    } deriving (Show, Generic)
makeLenses ''MUnitWord

fWORDNOTE :: MUnitWord -> Text
fWORDNOTE w = w^.fWORD <> match (w^.fNOTE) where
    match Nothing = ""
    match (Just "") = ""
    match (Just a) = "(" <> a <> ")"

data MUnitWords = MUnitWords { _fVUNITWORDS :: [MUnitWord] } deriving (Show, Generic)

instance ToJSON MUnitWord where
    toJSON = genericToJSON customOptionsLolly

instance FromJSON MUnitWord where
    parseJSON = genericParseJSON customOptionsLolly

instance FromJSON MUnitWords where
    parseJSON = genericParseJSON customOptionsLolly

getDataByTextbookUnitPart :: Int -> Int -> Int -> IO [MUnitWord]
getDataByTextbookUnitPart textbookid unitPartFrom unitPartTo = runReq def $ do
    v <- req GET (urlLolly /: "VUNITWORDS") NoReqBody jsonResponse $
        "transform" =: (1 :: Int) <>
        "filter[]" =: (printf "TEXTBOOKID,eq,%d" textbookid :: String) <>
        "filter[]" =: (printf "UNITPART,bt,%d,%d" unitPartFrom unitPartTo :: String) <>
        "order[]" =: ("UNITPART" :: String) <>
        "order[]" =: ("SEQNUM" :: String)
    return $ _fVUNITWORDS (responseBody v :: MUnitWords)

update :: Int -> MUnitWord -> IO (Maybe String)
update id item = runReq def $ do
    v <- req PUT (urlLolly /: "UNITWORDS" /~ id) (ReqBodyJson item) jsonResponse mempty
    return (responseBody v :: Maybe String)

create :: MUnitWord -> IO (Maybe String)
create item = runReq def $ do
    v <- req POST (urlLolly /: "UNITWORDS") (ReqBodyJson item) jsonResponse mempty
    return (responseBody v :: Maybe String)

delete :: Int -> IO (Maybe String)
delete id = runReq def $ do
    v <- req DELETE (urlLolly /: "UNITWORDS" /~ id) NoReqBody jsonResponse mempty
    return (responseBody v :: Maybe String)
