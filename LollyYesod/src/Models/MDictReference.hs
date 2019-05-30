{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.MDictReference
    ( MDictReference
    , fID
    , fDICTID
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

import Data.Aeson
import Data.Default.Class
import GHC.Generics
import Helpers
import Models.MDictionary
import Network.HTTP.Req

type MDictReference = MDictionary
newtype MDictsWord = MDictsWord{_fVDICTSMEAN :: [MDictReference]} deriving (Show, Generic)

instance FromJSON MDictsWord where
    parseJSON = genericParseJSON customOptionsLolly

getDataByLang :: Int -> IO [MDictReference]
getDataByLang langid = runReq def $ do
    v <- req GET (urlLolly /: "VDICTSMEAN") NoReqBody jsonResponse $
        "transform" =: (1 :: Int) <>
        "filter" =: ("LANGIDFROM,eq," ++ show langid)
    return $ _fVDICTSMEAN (responseBody v :: MDictsWord)
