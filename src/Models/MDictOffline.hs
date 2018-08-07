{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Models.MDictOffline
    ( MDictOffline
    , fID
    , fLANGIDFROM
    , fDICTTYPENAME
    , fDICTNAME
    , fURL
    , fCHCONV
    , fTRANSFORM_MAC
    , fWAIT
    , fTEMPLATE
    , getDictsOfflineByLang
    ) where

import Control.Lens
import Data.Aeson
import Data.Default.Class
import Data.Text (Text)
import GHC.Generics
import Helpers
import Models.MDictionary
import Network.HTTP.Req

type MDictOffline = MDictionary
data MDictsOffline = MDictsOffline { _fVDICTSOFFLINE :: [MDictOffline] } deriving (Show, Generic)

instance FromJSON MDictsOffline where
    parseJSON = genericParseJSON customOptionsLolly

getDictsOfflineByLang :: Int -> IO [MDictOffline]
getDictsOfflineByLang langid = runReq def $ do
    v <- req GET (urlLolly /: "VDICTSOFFLINE") NoReqBody jsonResponse $
        "transform" =: (1 :: Int) <>
        "filter" =: ("LANGIDFROM,eq," ++ show langid)
    return $ _fVDICTSOFFLINE (responseBody v :: MDictsOffline)
