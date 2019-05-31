{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.MDictTranslation
    ( MDictTranslation
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

import Data.Aeson
import Data.Default.Class
import GHC.Generics
import Helpers
import Models.MDictionary
import Network.HTTP.Req

type MDictTranslation = MDictionary
newtype MDictsTranslation = MDictsTranslation{_fVDICTSTRANSLATION :: [MDictTranslation]} deriving (Show, Generic)

instance FromJSON MDictsTranslation where
    parseJSON = genericParseJSON customOptionsLolly

getDataByLang :: Int -> IO [MDictTranslation]
getDataByLang langid = runReq def $ do
    v <- req GET (urlLolly /: "VDICTSTRANSLATION") NoReqBody jsonResponse $
        "transform" =: (1 :: Int) <>
        "filter" =: ("LANGIDFROM,eq," ++ show langid)
    return $ _fVDICTSTRANSLATION (responseBody v)
