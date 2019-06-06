{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.MDictReference
    ( MDictReference
    , fID
    , fDICTID
    , fLANGIDFROM
    , fLANGNAMEFROM
    , fLANGIDTO
    , fLANGNAMETO
    , fSEQNUM
    , fDICTTYPENAME
    , fDICTNAME
    , fURL
    , fCHCONV
    , fAUTOMATION
    , fDICTTABLE
    , fTRANSFORM_WIN
    , fTRANSFORM
    , fWAIT
    , fTEMPLATE
    , fTEMPLATE2
    , getDataByLang
    ) where

import Data.Aeson
import Data.Default
import GHC.Generics
import Helpers
import Models.MDictionary
import Network.HTTP.Req

type MDictReference = MDictionary
newtype MDictsReference = MDictsWord{records :: [MDictReference]} deriving (Show, Generic)

instance FromJSON MDictsReference

getDataByLang :: Int -> IO [MDictReference]
getDataByLang langid = runReq def $ do
    v <- req GET (urlLolly /: "VDICTSMEAN") NoReqBody jsonResponse $
        "filter" =: ("LANGIDFROM,eq," ++ show langid)
    return $ records (responseBody v :: MDictsReference)
