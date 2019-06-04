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

type MDictReference = MDictionary
newtype MDictsWord = MDictsWord{_frecords :: [MDictReference]} deriving (Show, Generic)

instance FromJSON MDictsWord where
    parseJSON = genericParseJSON customOptionsLolly

getDataByLang :: Int -> IO [MDictReference]
getDataByLang langid = runReq def $ do
    v <- req GET (urlLolly /: "VDICTSMEAN") NoReqBody jsonResponse $
        "filter" =: ("LANGIDFROM,eq," ++ show langid)
    return $ _frecords (responseBody v :: MDictsWord)
