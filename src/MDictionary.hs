{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module MDictionary
    ( MDictOnline
    , MDictOffline
    , MDictNote
    , fID
    , fLANGIDFROM
    , fDICTTYPENAME
    , fDICTNAME
    , fURL
    , fCHCONV
    , fTRANSFORM_MAC
    , fWAIT
    , fTEMPLATE
    , getDictsOnlineByLang
    , getDictsOfflineByLang
    , getDictsNoteByLang
    ) where

import Control.Lens
import Data.Aeson
import Data.Default.Class
import Data.Text (Text)
import GHC.Generics
import Helpers
import Network.HTTP.Req

data MDictionary = MDictionary
    { _fID :: Int
    , _fLANGIDFROM :: Int
    , _fDICTTYPENAME :: Text
    , _fDICTNAME :: Text
    , _fURL :: Maybe Text
    , _fCHCONV :: Maybe Text
    , _fTRANSFORM_MAC :: Maybe Text
    , _fWAIT :: Maybe Int
    , _fTEMPLATE :: Maybe Text
    } deriving (Show, Generic)
makeLenses ''MDictionary

type MDictOnline = MDictionary
type MDictOffline = MDictionary
type MDictNote = MDictionary

data MDictsOnline = MDictsOnline { _fVDICTSONLINE :: [MDictOnline] } deriving (Show, Generic)
data MDictsOffline = MDictsOffline { _fVDICTSOFFLINE :: [MDictOffline] } deriving (Show, Generic)
data MDictsNote = MDictsNote { _fVDICTSNOTE :: [MDictNote] } deriving (Show, Generic)

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
    return $ _fVDICTSONLINE (responseBody v :: MDictsOnline)

getDictsOfflineByLang :: Int -> IO [MDictOffline]
getDictsOfflineByLang langid = runReq def $ do
    v <- req GET (urlLolly /: "VDICTSOFFLINE") NoReqBody jsonResponse $
        "transform" =: (1 :: Int) <>
        "filter" =: ("LANGIDFROM,eq," ++ show langid)
    return $ _fVDICTSOFFLINE (responseBody v :: MDictsOffline)

getDictsNoteByLang :: Int -> IO [MDictNote]
getDictsNoteByLang langid = runReq def $ do
    v <- req GET (urlLolly /: "VDICTSNOTE") NoReqBody jsonResponse $
        "transform" =: (1 :: Int) <>
        "filter" =: ("LANGIDFROM,eq," ++ show langid)
    return $ _fVDICTSNOTE (responseBody v)
