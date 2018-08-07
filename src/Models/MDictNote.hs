{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Models.MDictNote
    ( MDictNote
    , fID
    , fLANGIDFROM
    , fDICTTYPENAME
    , fDICTNAME
    , fURL
    , fCHCONV
    , fTRANSFORM_MAC
    , fWAIT
    , fTEMPLATE
    , getDictsNoteByLang
    ) where

import Control.Lens
import Data.Aeson
import Data.Default.Class
import Data.Text (Text)
import GHC.Generics
import Helpers
import Models.MDictionary
import Network.HTTP.Req

type MDictNote = MDictionary
data MDictsNote = MDictsNote { _fVDICTSNOTE :: [MDictNote] } deriving (Show, Generic)

instance FromJSON MDictsNote where
    parseJSON = genericParseJSON customOptionsLolly

getDictsNoteByLang :: Int -> IO [MDictNote]
getDictsNoteByLang langid = runReq def $ do
    v <- req GET (urlLolly /: "VDICTSNOTE") NoReqBody jsonResponse $
        "transform" =: (1 :: Int) <>
        "filter" =: ("LANGIDFROM,eq," ++ show langid)
    return $ _fVDICTSNOTE (responseBody v)
