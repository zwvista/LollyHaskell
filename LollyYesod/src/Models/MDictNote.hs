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
    , fAUTOJUMP
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
newtype MDictsNote = MDictsNote{_fVDICTSNOTE :: [MDictNote]} deriving (Show, Generic)

instance FromJSON MDictsNote where
    parseJSON = genericParseJSON customOptionsLolly

getDataByLang :: Int -> IO [MDictNote]
getDataByLang langid = runReq def $ do
    v <- req GET (urlLolly /: "VDICTSNOTE") NoReqBody jsonResponse $
        "transform" =: (1 :: Int) <>
        "filter" =: ("LANGIDFROM,eq," ++ show langid)
    return $ _fVDICTSNOTE (responseBody v)
