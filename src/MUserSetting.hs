{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module MUserSetting
    ( MUserSetting
    , fID
    , fUSERID
    , fKIND
    , fENTITYID
    , fVALUE1
    , fVALUE2
    , fVALUE3
    , fVALUE4
    , getUserSettingsByUser
    , updateUserSettingLang
    , updateUserSettingTextbook
    , updateUserSettingDictOnline
    , updateUserSettingDictNote
    , updateUserSettingUnitFrom
    , updateUserSettingPartFrom
    , updateUserSettingUnitTo
    , updateUserSettingPartTo
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

getUserSettingsByUser :: Int -> IO [MUserSetting]
getUserSettingsByUser userid = runReq def $ do
    v <- req GET (urlLolly /: "USERSETTINGS") NoReqBody jsonResponse $
        "transform" =: (1 :: Int) <>
        "filter" =: (printf "USERID,eq,%d" userid :: String)
    return $ _fUSERSETTINGS (responseBody v :: MUserSettings)

updateUserSetting :: Int -> String -> IO (Maybe String)
updateUserSetting id str = runReq def $ do
    v <- req PUT (urlLolly /: "USERSETTINGS" /~ id) (ReqBodyLbs (B.fromString str)) jsonResponse mempty
    return (responseBody v :: Maybe String)

updateUserSettingLang :: Int -> Int -> IO (Maybe String)
updateUserSettingLang id langid = updateUserSetting id ("VALUE1=" ++ show langid)

updateUserSettingTextbook :: Int -> Int -> IO (Maybe String)
updateUserSettingTextbook id textbookid = updateUserSetting id ("VALUE1=" ++ show textbookid)

updateUserSettingDictOnline :: Int -> Int -> IO (Maybe String)
updateUserSettingDictOnline id dictonlineid = updateUserSetting id ("VALUE2=" ++ show dictonlineid)

updateUserSettingDictNote :: Int -> Int -> IO (Maybe String)
updateUserSettingDictNote id dictnoteid = updateUserSetting id ("VALUE3=" ++ show dictnoteid)

updateUserSettingUnitFrom :: Int -> Int -> IO (Maybe String)
updateUserSettingUnitFrom id usunitfrom = updateUserSetting id ("VALUE1=" ++ show usunitfrom)

updateUserSettingPartFrom :: Int -> Int -> IO (Maybe String)
updateUserSettingPartFrom id uspartfrom = updateUserSetting id ("VALUE2=" ++ show uspartfrom)

updateUserSettingUnitTo :: Int -> Int -> IO (Maybe String)
updateUserSettingUnitTo id usunitto = updateUserSetting id ("VALUE3=" ++ show usunitto)

updateUserSettingPartTo :: Int -> Int -> IO (Maybe String)
updateUserSettingPartTo id uspartto = updateUserSetting id ("VALUE4=" ++ show uspartto)
