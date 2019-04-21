{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.MDictMean
    ( MDictMean
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

import Control.Lens
import Data.Aeson
import Data.Default.Class
import Data.Text (Text)
import GHC.Generics
import Helpers
import Models.MDictionary
import Network.HTTP.Req

type MDictMean = MDictionary
newtype MDictsWord = MDictsWord{_fVDICTSMEAN :: [MDictMean]} deriving (Show, Generic)

instance FromJSON MDictsWord where
    parseJSON = genericParseJSON customOptionsLolly

getDataByLang :: Int -> IO [MDictMean]
getDataByLang langid = runReq def $ do
    v <- req GET (urlLolly /: "VDICTSMEAN") NoReqBody jsonResponse $
        "transform" =: (1 :: Int) <>
        "filter" =: ("LANGIDFROM,eq," ++ show langid)
    return $ _fVDICTSMEAN (responseBody v :: MDictsWord)
