{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.MDictNote
    ( MDictNote
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

type MDictNote = MDictionary
newtype MDictsNote = MDictsNote{records :: [MDictNote]} deriving (Show, Generic)

instance FromJSON MDictsNote

getDataByLang :: Int -> IO [MDictNote]
getDataByLang langid = runReq def $ do
    v <- req GET (urlLolly /: "VDICTSNOTE") NoReqBody jsonResponse $
        "filter" =: ("LANGIDFROM,eq," ++ show langid)
    return $ records (responseBody v)
