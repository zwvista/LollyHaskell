{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module MLangWord
    ( MLangWord
    , MLangWords
    , getLangWordsByLang
    , updateLangWord
    , deleteLangWord
    ) where

import Data.Aeson
import Data.Default.Class
import Data.Text (Text)
import GHC.Generics
import Helpers
import Network.HTTP.Req

data MLangWord = MLangWord
    { fID :: Int
    , fLANGID :: Int
    , fWORD :: Text
    , fTRANSLATION :: Text
    } deriving (Show, Generic)

data MLangWords = MLangWords { fLANGWORDS :: [MLangWord] } deriving (Show, Generic)

instance ToJSON MLangWord where
    toJSON = genericToJSON customOptionsLolly

instance FromJSON MLangWord where
    parseJSON = genericParseJSON customOptionsLolly

instance FromJSON MLangWords where
    parseJSON = genericParseJSON customOptionsLolly

getLangWordsByLang :: Int -> IO [MLangWord]
getLangWordsByLang langid = runReq def $ do
    v <- req GET (urlLolly /: "LANGWORDS") NoReqBody jsonResponse $
        "transform" =: (1 :: Int) <>
        "filter" =: ("LANGID,eq," ++ show langid)
    return $ fLANGWORDS (responseBody v :: MLangWords)

updateLangWord :: Int -> MLangWord -> IO (Maybe String)
updateLangWord id item = runReq def $ do
    v <- req PUT (urlLolly /: "LANGWORDS" /~ id) (ReqBodyJson item) jsonResponse mempty
    return (responseBody v :: Maybe String)

createLangWord :: MLangWord -> IO (Maybe String)
createLangWord item = runReq def $ do
    v <- req POST (urlLolly /: "LANGWORDS") (ReqBodyJson item) jsonResponse mempty
    return (responseBody v :: Maybe String)

deleteLangWord :: Int -> IO (Maybe String)
deleteLangWord id = runReq def $ do
    v <- req DELETE (urlLolly /: "LANGWORDS" /~ id) NoReqBody jsonResponse mempty
    return (responseBody v :: Maybe String)
