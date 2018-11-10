{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Models.MDictOnline
    ( MDictOnline
    , fID
    , fLANGIDFROM
    , fDICTTYPENAME
    , fDICTNAME
    , fURL
    , fCHCONV
    , fTRANSFORM
    , fWAIT
    , fTEMPLATE
    , getDataByLang
    ) where

import Control.Lens
import Data.Aeson
import Data.Default.Class
import Data.Text (Text)
import GHC.Generics
import Helpers
import Models.MDictionary
import Network.HTTP.Req

type MDictOnline = MDictionary
data MDictsOnline = MDictsOnline { _fVDICTSONLINE :: [MDictOnline] } deriving (Show, Generic)

instance FromJSON MDictsOnline where
    parseJSON = genericParseJSON customOptionsLolly

getDataByLang :: Int -> IO [MDictOnline]
getDataByLang langid = runReq def $ do
    v <- req GET (urlLolly /: "VDICTSONLINE") NoReqBody jsonResponse $
        "transform" =: (1 :: Int) <>
        "filter" =: ("LANGIDFROM,eq," ++ show langid)
    return $ _fVDICTSONLINE (responseBody v :: MDictsOnline)
