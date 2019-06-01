{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
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
    , updateDictItem
    , updateDictNote
    , updateUnitFrom
    , updatePartFrom
    , updateUnitTo
    , updatePartTo
    ) where

import Control.Lens
import Data.Aeson
import qualified Data.ByteString.Lazy.UTF8 as B
import Data.Default
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
    } deriving (Show, Generic, Default)
makeLenses ''MUserSetting

newtype MUserSettings = MUserSettings{_fUSERSETTINGS :: [MUserSetting]} deriving (Show, Generic)

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
update fid str = runReq def $ do
    v <- req PUT (urlLolly /: "USERSETTINGS" /~ fid) (ReqBodyLbs (B.fromString str)) jsonResponse mempty
    return (responseBody v :: Maybe String)

updateLang :: Int -> Int -> IO (Maybe String)
updateLang fid langid = update fid ("VALUE1=" ++ show langid)

updateTextbook :: Int -> Int -> IO (Maybe String)
updateTextbook fid textbookid = update fid ("VALUE1=" ++ show textbookid)

updateDictItem :: Int -> Int -> IO (Maybe String)
updateDictItem fid dictpicker = update fid ("VALUE2=" ++ show dictpicker)

updateDictNote :: Int -> Int -> IO (Maybe String)
updateDictNote fid dictnoteid = update fid ("VALUE3=" ++ show dictnoteid)

updateUnitFrom :: Int -> Int -> IO (Maybe String)
updateUnitFrom fid usunitfrom = update fid ("VALUE1=" ++ show usunitfrom)

updatePartFrom :: Int -> Int -> IO (Maybe String)
updatePartFrom fid uspartfrom = update fid ("VALUE2=" ++ show uspartfrom)

updateUnitTo :: Int -> Int -> IO (Maybe String)
updateUnitTo fid usunitto = update fid ("VALUE3=" ++ show usunitto)

updatePartTo :: Int -> Int -> IO (Maybe String)
updatePartTo fid uspartto = update fid ("VALUE4=" ++ show uspartto)
