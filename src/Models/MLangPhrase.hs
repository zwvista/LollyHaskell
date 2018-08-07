{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Models.MLangPhrase
    ( MLangPhrase
    , fID
    , fLANGID
    , fPHRASE
    , fTRANSLATION
    , getLangPhrasesByLang
    , updateLangPhrase
    , deleteLangPhrase
    ) where

import Control.Lens
import Data.Aeson
import Data.Default.Class
import Data.Text (Text)
import GHC.Generics
import Helpers
import Network.HTTP.Req

data MLangPhrase = MLangPhrase
    { _fID :: Int
    , _fLANGID :: Int
    , _fPHRASE :: Text
    , _fTRANSLATION :: Text
    } deriving (Show, Generic)
makeLenses ''MLangPhrase

data MLangPhrases = MLangPhrases { _fLANGPHRASES :: [MLangPhrase] } deriving (Show, Generic)

instance ToJSON MLangPhrase where
    toJSON = genericToJSON customOptionsLolly

instance FromJSON MLangPhrase where
    parseJSON = genericParseJSON customOptionsLolly

instance FromJSON MLangPhrases where
    parseJSON = genericParseJSON customOptionsLolly

getLangPhrasesByLang :: Int -> IO [MLangPhrase]
getLangPhrasesByLang langid = runReq def $ do
    v <- req GET (urlLolly /: "LANGPHRASES") NoReqBody jsonResponse $
        "transform" =: (1 :: Int) <>
        "filter" =: ("LANGID,eq," ++ show langid)
    return $ _fLANGPHRASES (responseBody v :: MLangPhrases)

updateLangPhrase :: Int -> MLangPhrase -> IO (Maybe String)
updateLangPhrase id item = runReq def $ do
    v <- req PUT (urlLolly /: "LANGPHRASES" /~ id) (ReqBodyJson item) jsonResponse mempty
    return (responseBody v :: Maybe String)

createLangPhrase :: MLangPhrase -> IO (Maybe String)
createLangPhrase item = runReq def $ do
    v <- req POST (urlLolly /: "LANGPHRASES") (ReqBodyJson item) jsonResponse mempty
    return (responseBody v :: Maybe String)

deleteLangPhrase :: Int -> IO (Maybe String)
deleteLangPhrase id = runReq def $ do
    v <- req DELETE (urlLolly /: "LANGPHRASES" /~ id) NoReqBody jsonResponse mempty
    return (responseBody v :: Maybe String)
