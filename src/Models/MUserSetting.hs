{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Models.MUserSetting
    ( MUserSetting
    , fID
    , fUSERID
    , fKIND
    , fENTITYID
    , fVALUE1
    , fVALUE2
    , fVALUE3
    , fVALUE4
    , getDataByUser
    , updateLang
    , updateTextbook
    , updateDictOnline
    , updateDictNote
    , updateUnitFrom
    , updatePartFrom
    , updateUnitTo
    , updatePartTo
    ) where

import Control.Lens
import Data.Aeson
import qualified Data.ByteString.Lazy.UTF8 as B
import Data.Default.Class
import Data.Text (Text)
import GHC.Generics
import Helpers
import Network.HTTP.Req
import Text.Printf

data MUserSetting = MUserSetting
    { _fID :: Int
    , _fUSERID :: Int
    , _fKIND :: Int
    , _fENTITYID :: Int
    , _fVALUE1 :: Maybe Text
    , _fVALUE2 :: Maybe Text
    , _fVALUE3 :: Maybe Text
    , _fVALUE4 :: Maybe Text
    } deriving (Show, Generic)
makeLenses ''MUserSetting

data MUserSettings = MUserSettings { _fUSERSETTINGS :: [MUserSetting] } deriving (Show, Generic)

instance ToJSON MUserSetting where
    toJSON = genericToJSON customOptionsLolly

instance FromJSON MUserSetting where
    parseJSON = genericParseJSON customOptionsLolly

instance FromJSON MUserSettings where
    parseJSON = genericParseJSON customOptionsLolly

getDataByUser :: Int -> IO [MUserSetting]
getDataByUser userid = runReq def $ do
    v <- req GET (urlLolly /: "USERSETTINGS") NoReqBody jsonResponse $
        "transform" =: (1 :: Int) <>
        "filter" =: (printf "USERID,eq,%d" userid :: String)
    return $ _fUSERSETTINGS (responseBody v :: MUserSettings)

update :: Int -> String -> IO (Maybe String)
update id str = runReq def $ do
    v <- req PUT (urlLolly /: "USERSETTINGS" /~ id) (ReqBodyLbs (B.fromString str)) jsonResponse mempty
    return (responseBody v :: Maybe String)

updateLang :: Int -> Int -> IO (Maybe String)
updateLang id langid = update id ("VALUE1=" ++ show langid)

updateTextbook :: Int -> Int -> IO (Maybe String)
updateTextbook id textbookid = update id ("VALUE1=" ++ show textbookid)

updateDictOnline :: Int -> Int -> IO (Maybe String)
updateDictOnline id dictonlineid = update id ("VALUE2=" ++ show dictonlineid)

updateDictNote :: Int -> Int -> IO (Maybe String)
updateDictNote id dictnoteid = update id ("VALUE3=" ++ show dictnoteid)

updateUnitFrom :: Int -> Int -> IO (Maybe String)
updateUnitFrom id usunitfrom = update id ("VALUE1=" ++ show usunitfrom)

updatePartFrom :: Int -> Int -> IO (Maybe String)
updatePartFrom id uspartfrom = update id ("VALUE2=" ++ show uspartfrom)

updateUnitTo :: Int -> Int -> IO (Maybe String)
updateUnitTo id usunitto = update id ("VALUE3=" ++ show usunitto)

updatePartTo :: Int -> Int -> IO (Maybe String)
updatePartTo id uspartto = update id ("VALUE4=" ++ show uspartto)
