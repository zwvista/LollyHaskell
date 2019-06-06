{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.MDictTranslation
    ( MDictTranslation
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

type MDictTranslation = MDictionary
newtype MDictsTranslation = MDictsTranslation{records :: [MDictTranslation]} deriving (Show, Generic)

instance FromJSON MDictsTranslation

getDataByLang :: Int -> IO [MDictTranslation]
getDataByLang langid = runReq def $ do
    v <- req GET (urlLolly /: "VDICTSTRANSLATION") NoReqBody jsonResponse $
        "filter" =: ("LANGIDFROM,eq," ++ show langid)
    return $ records (responseBody v)
