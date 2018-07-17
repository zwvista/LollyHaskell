{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module MDictionary
    ( MDictOnline
    , MDictOffline
    , MDictNote
    , MDictsOnline
    , MDictsOffline
    , MDictsNote
    , getDictsOnlineByLang
    , getDictsOfflineByLang
    , getDictsNoteByLang
    ) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Req
import Data.Default.Class
import Helpers

data MDictionary = MDictionary
    { fID :: Int
    , fLANGIDFROM :: Int
    , fDICTTYPENAME :: Text
    , fDICTNAME :: Text
    , fURL :: Maybe Text
    , fCHCONV :: Maybe Text
    , fTRANSFORM_MAC :: Maybe Text
    , fWAIT :: Maybe Int
    , fTEMPLATE :: Maybe Text
    } deriving (Show, Generic)

type MDictOnline = MDictionary
type MDictOffline = MDictionary
type MDictNote = MDictionary

data MDictsOnline = MDictsOnline { fVDICTSONLINE :: [MDictOnline] } deriving (Show, Generic)
data MDictsOffline = MDictsOffline { fVDICTSOFFLINE :: [MDictOffline] } deriving (Show, Generic)
data MDictsNote = MDictsNote { fVDICTSNOTE :: [MDictNote] } deriving (Show, Generic)

instance ToJSON MDictionary where
    toJSON = genericToJSON customOptionsLolly

instance FromJSON MDictionary where
    parseJSON = genericParseJSON customOptionsLolly

instance FromJSON MDictsOnline where
    parseJSON = genericParseJSON customOptionsLolly

instance FromJSON MDictsOffline where
    parseJSON = genericParseJSON customOptionsLolly

instance FromJSON MDictsNote where
    parseJSON = genericParseJSON customOptionsLolly

getDictsOnlineByLang :: Int -> IO [MDictOnline]
getDictsOnlineByLang langid = runReq def $ do
    v <- req GET (urlLolly /: "VDICTSONLINE") NoReqBody jsonResponse $
        "transform" =: (1 :: Int) <>
        "filter" =: ("LANGIDFROM,eq," ++ show langid)
    return $ fVDICTSONLINE (responseBody v :: MDictsOnline)

getDictsOfflineByLang :: Int -> IO [MDictOffline]
getDictsOfflineByLang langid = runReq def $ do
    v <- req GET (urlLolly /: "VDICTSOFFLINE") NoReqBody jsonResponse $
        "transform" =: (1 :: Int) <>
        "filter" =: ("LANGIDFROM,eq," ++ show langid)
    return $ fVDICTSOFFLINE (responseBody v :: MDictsOffline)

getDictsNoteByLang :: Int -> IO [MDictNote]
getDictsNoteByLang langid = runReq def $ do
    v <- req GET (urlLolly /: "VDICTSNOTE") NoReqBody jsonResponse $
        "transform" =: (1 :: Int) <>
        "filter" =: ("LANGIDFROM,eq," ++ show langid)
    return $ fVDICTSNOTE (responseBody v :: MDictsNote)
