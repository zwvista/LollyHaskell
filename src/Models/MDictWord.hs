{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Models.MDictWord
    ( MDictWord
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

type MDictWord = MDictionary
data MDictsWord = MDictsWord { _fVDICTSWORD :: [MDictWord] } deriving (Show, Generic)

instance FromJSON MDictsWord where
    parseJSON = genericParseJSON customOptionsLolly

getDataByLang :: Int -> IO [MDictWord]
getDataByLang langid = runReq def $ do
    v <- req GET (urlLolly /: "VDICTSWORD") NoReqBody jsonResponse $
        "transform" =: (1 :: Int) <>
        "filter" =: ("LANGIDFROM,eq," ++ show langid)
    return $ _fVDICTSWORD (responseBody v :: MDictsWord)
