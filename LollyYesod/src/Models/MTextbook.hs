{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Models.MTextbook
    ( MTextbook
    , fID
    , fLANGID
    , fTEXTBOOKNAME
    , fUNITS
    , fPARTS
    , getDataByLang
    ) where

import Control.Lens
import Data.Aeson
import Data.Default.Class
import Data.Text (Text)
import GHC.Generics
import Helpers
import Network.HTTP.Req

data MTextbook = MTextbook
    { _fID :: Int
    , _fLANGID :: Int
    , _fTEXTBOOKNAME :: Text
    , _fUNITS :: Int
    , _fPARTS :: Text
    } deriving (Show, Generic)
makeLenses ''MTextbook

newtype MTextbooks = MTextbooks{_fTEXTBOOKS :: [MTextbook]} deriving (Show, Generic)

customOptions :: Options
customOptions = aesonDrop 2 $ \case
    "TEXTBOOKNAME" -> "NAME"
    n -> n

instance ToJSON MTextbook where
    toJSON = genericToJSON customOptions

instance FromJSON MTextbook where
    parseJSON = genericParseJSON customOptions

instance FromJSON MTextbooks where
    parseJSON = genericParseJSON customOptions

getDataByLang :: Int -> IO [MTextbook]
getDataByLang langid = runReq def $ do
    v <- req GET (urlLolly /: "TEXTBOOKS") NoReqBody jsonResponse $
        "transform" =: (1 :: Int) <>
        "filter" =: ("LANGID,eq," ++ show langid)
    return $ _fTEXTBOOKS (responseBody v :: MTextbooks)
