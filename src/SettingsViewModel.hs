{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module SettingsViewModel
    ( SettingsViewModel
    , arrUserSettings
    , selectedUSUserIndex
    , selectedUSLangIndex
    , selectedUSTextbookIndex
    , arrLanguages
    , selectedLangIndex
    , arrDictsOnline
    , selectedDictOnlineIndex
    , arrDictsNote
    , selectedDictNoteIndex
    , arrTextbooks
    , selectedTextbookIndex
    , SettingsViewModel.getData
    ) where

import Control.Concurrent.Async
import Control.Lens
import Control.Monad
import Control.Arrow
import Data.Default.Class
import Data.List
import Data.Maybe
import Data.Text (Text)
import Data.Text.Read
import Formatting
import GHC.Generics
import Models.MDictNote
import Models.MDictOnline
import Models.MLanguage
import Models.MTextbook
import Models.MUserSetting

data SettingsViewModel = SettingsViewModel
    { _arrUserSettings :: [MUserSetting]
    , _selectedUSUserIndex :: Maybe Int
    , _selectedUSLangIndex :: Maybe Int
    , _selectedUSTextbookIndex :: Maybe Int
    , _arrLanguages :: [MLanguage]
    , _selectedLangIndex :: Maybe Int
    , _arrDictsOnline :: [MDictOnline]
    , _selectedDictOnlineIndex :: Maybe Int
    , _arrDictsNote :: [MDictNote]
    , _selectedDictNoteIndex :: Maybe Int
    , _arrTextbooks :: [MTextbook]
    , _selectedTextbookIndex :: Maybe Int
    } deriving (Show, Generic, Default)
makeLenses ''SettingsViewModel

getUSXXID :: Lens' SettingsViewModel (Maybe Int) -> Lens' MUserSetting (Maybe Text) -> SettingsViewModel -> Maybe Int
getUSXXID fIndex fValue vm = vm ^. arrUserSettings ^? ix (vm ^. fIndex . non (-1)) >>= (^. fValue) >>= (decimal >>> (^? _Right)) <&> fst
setUSXXID :: Lens' SettingsViewModel (Maybe Int) -> Lens' MUserSetting (Maybe Text) -> Int -> SettingsViewModel -> SettingsViewModel
setUSXXID fIndex fValue id vm = vm & arrUserSettings . ix (vm ^. fIndex . non (-1)) . fValue . _Just .~ (sformat int id)

getUSLANGID :: SettingsViewModel -> Maybe Int
getUSLANGID = getUSXXID selectedUSUserIndex fVALUE1
setUSLANGID :: Int -> SettingsViewModel -> SettingsViewModel
setUSLANGID id = setUSXXID selectedUSUserIndex fVALUE1 id

getUSTEXTBOOKID :: SettingsViewModel -> Maybe Int
getUSTEXTBOOKID = getUSXXID selectedUSLangIndex fVALUE1
setUSTEXTBOOKID :: Int -> SettingsViewModel -> SettingsViewModel
setUSTEXTBOOKID id = setUSXXID selectedUSLangIndex fVALUE1 id

getUSDICTONLINEID :: SettingsViewModel -> Maybe Int
getUSDICTONLINEID = getUSXXID selectedUSLangIndex fVALUE2
setUSDICTONLINEID :: Int -> SettingsViewModel -> SettingsViewModel
setUSDICTONLINEID id = setUSXXID selectedUSLangIndex fVALUE2 id

getUSDICTNOTEID :: SettingsViewModel -> Maybe Int
getUSDICTNOTEID = getUSXXID selectedUSLangIndex fVALUE3
setUSDICTNOTEID :: Int -> SettingsViewModel -> SettingsViewModel
setUSDICTNOTEID id = setUSXXID selectedUSLangIndex fVALUE3 id

getUSUNITFROM :: SettingsViewModel -> Maybe Int
getUSUNITFROM = getUSXXID selectedUSTextbookIndex fVALUE1
setUSUNITFROM :: Int -> SettingsViewModel -> SettingsViewModel
setUSUNITFROM id = setUSXXID selectedUSTextbookIndex fVALUE1 id

getUSPARTFROM :: SettingsViewModel -> Maybe Int
getUSPARTFROM = getUSXXID selectedUSTextbookIndex fVALUE2
setUSPARTFROM :: Int -> SettingsViewModel -> SettingsViewModel
setUSPARTFROM id = setUSXXID selectedUSTextbookIndex fVALUE2 id

getUSUNITTO :: SettingsViewModel -> Maybe Int
getUSUNITTO = getUSXXID selectedUSTextbookIndex fVALUE3
setUSUNITTO :: Int -> SettingsViewModel -> SettingsViewModel
setUSUNITTO id = setUSXXID selectedUSTextbookIndex fVALUE3 id

getUSPARTTO :: SettingsViewModel -> Maybe Int
getUSPARTTO = getUSXXID selectedUSTextbookIndex fVALUE4
setUSPARTTO :: Int -> SettingsViewModel -> SettingsViewModel
setUSPARTTO id = setUSXXID selectedUSTextbookIndex fVALUE4 id

getUSUNITPARTFROM :: SettingsViewModel -> Maybe Int
getUSUNITPARTFROM vm = (+) <$> (*10) <$> getUSUNITFROM vm <*> getUSPARTFROM vm

getUSUNITPARTTO :: SettingsViewModel -> Maybe Int
getUSUNITPARTTO vm = (+) <$> (*10) <$> getUSUNITTO vm <*> getUSPARTTO vm

isSingleUnitPart :: SettingsViewModel -> Maybe Bool
isSingleUnitPart vm = (==) <$> getUSUNITPARTFROM vm <*> getUSUNITPARTTO vm

isInvalidUnitPart :: SettingsViewModel -> Maybe Bool
isInvalidUnitPart vm = (>) <$> getUSUNITPARTFROM vm <*> getUSUNITPARTTO vm

getData :: IO SettingsViewModel
getData = do
    (r1, r2) <- concurrently (Models.MLanguage.getData) (Models.MUserSetting.getDataByUser 1)
    return $ (def :: SettingsViewModel)
        & arrLanguages .~ r1
        & arrUserSettings .~ r2
        & selectedUSUserIndex .~ (findIndex (\o -> o ^. fKIND == 1) r2)
