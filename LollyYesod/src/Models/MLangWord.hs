{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Models.MLangWord
    ( MLangWord
    , fID
    , fLANGID
    , fWORD
    , fTRANSLATION
    , getDataByLang
    , update
    , create
    , delete
    ) where

import Control.Lens
import Data.Aeson
import Data.Default.Class
import Data.Text (Text)
import GHC.Generics
import Helpers
import Network.HTTP.Req

data MLangWord = MLangWord
    { _fID :: Int
    , _fLANGID :: Int
    , _fWORD :: Text
    , _fTRANSLATION :: Text
    } deriving (Show, Generic)
makeLenses ''MLangWord

newtype MLangWords = MLangWords{_fLANGWORDS :: [MLangWord]} deriving (Show, Generic)

instance ToJSON MLangWord where
    toJSON = genericToJSON customOptionsLolly

instance FromJSON MLangWord where
    parseJSON = genericParseJSON customOptionsLolly

instance FromJSON MLangWords where
    parseJSON = genericParseJSON customOptionsLolly

getDataByLang :: Int -> IO [MLangWord]
getDataByLang langid = runReq def $ do
    v <- req GET (urlLolly /: "LANGWORDS") NoReqBody jsonResponse $
        "transform" =: (1 :: Int) <>
        "filter" =: ("LANGID,eq," ++ show langid)
    return $ _fLANGWORDS (responseBody v :: MLangWords)

update :: Int -> MLangWord -> IO (Maybe String)
update id item = runReq def $ do
    v <- req PUT (urlLolly /: "LANGWORDS" /~ id) (ReqBodyJson item) jsonResponse mempty
    return (responseBody v :: Maybe String)

create :: MLangWord -> IO (Maybe String)
create item = runReq def $ do
    v <- req POST (urlLolly /: "LANGWORDS") (ReqBodyJson item) jsonResponse mempty
    return (responseBody v :: Maybe String)

delete :: Int -> IO (Maybe String)
delete id = runReq def $ do
    v <- req DELETE (urlLolly /: "LANGWORDS" /~ id) NoReqBody jsonResponse mempty
    return (responseBody v :: Maybe String)