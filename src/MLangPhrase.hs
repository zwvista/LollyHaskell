{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module MLangPhrase
    ( MLangPhrase
    , MLangPhrases
    ) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Req
import Data.Default.Class
import Helpers

data MLangPhrase = MLangPhrase
    { fID :: Int
    , fLANGID :: Int
    , fPHRASE :: Text
    , fTRANSLATION :: Text
    } deriving (Show, Generic)

data MLangPhrases = MLangPhrases { fLANGPHRASES :: [MLangPhrase] } deriving (Show, Generic)

instance ToJSON MLangPhrase where
    toJSON = genericToJSON customOptionsLolly

instance FromJSON MLangPhrase where
    parseJSON = genericParseJSON customOptionsLolly

instance FromJSON MLangPhrases where
    parseJSON = genericParseJSON customOptionsLolly

getDataByLang :: Int -> IO [MLangPhrase]
getDataByLang langid = runReq def $ do
    v <- req GET (urlLolly /: "LANGPHRASES") NoReqBody jsonResponse $
        "transform" =: (1 :: Int) <>
        "filter" =: ("LANGID,eq," ++ show langid)
    return $ fLANGPHRASES (responseBody v :: MLangPhrases)
