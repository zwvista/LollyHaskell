{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module SettingsViewModel
    (
    ) where

import Control.Lens
import Control.Monad
import Control.Arrow
import Data.Text (Text)
import Data.Text.Read
import Formatting
import MDictionary
import MLanguage
import MTextbook
import MUserSetting

data SettingsViewModel = SettingsViewModel
    { _arrUserSettings :: [MUserSetting]
    , _selectedUSUserIndex :: Int
    , _selectedUSLangIndex :: Int
    , _selectedUSTextbookIndex :: Int
    , _arrLanguages :: [MLanguage]
    , _selectedLangIndex :: Int
    , _arrDictsOnline :: [MDictOnline]
    , _selectedDictOnlineIndex :: Int
    , _arrDictsNote :: [MDictNote]
    , _selectedDictNoteIndex :: Int
    , _arrTextbooks :: [MTextbook]
    , _selectedTextbookIndex :: Int
    }
makeLenses ''SettingsViewModel

getUSXXID :: SettingsViewModel -> Lens' SettingsViewModel Int -> Lens' MUserSetting (Maybe Text) -> Maybe Int
getUSXXID vm fIndex fValue = vm ^. arrUserSettings ^? ix (vm ^. fIndex) >>= (^. fValue) >>= (decimal >>> (^? _Right)) <&> fst
setUSXXID :: SettingsViewModel -> Lens' SettingsViewModel Int -> Lens' MUserSetting (Maybe Text) -> Int -> SettingsViewModel
setUSXXID vm fIndex fValue id = vm & arrUserSettings . ix (vm ^. fIndex) . fValue . _Just .~ (sformat int id)

getUSLANGID :: SettingsViewModel -> Maybe Int
getUSLANGID vm = getUSXXID vm selectedUSUserIndex fVALUE1
setUSLANGID :: SettingsViewModel -> Int -> SettingsViewModel
setUSLANGID vm id = setUSXXID vm selectedUSUserIndex fVALUE1 id

getUSTEXTBOOKID :: SettingsViewModel -> Maybe Int
getUSTEXTBOOKID vm = getUSXXID vm selectedUSLangIndex fVALUE1
setUSTEXTBOOKID :: SettingsViewModel -> Int -> SettingsViewModel
setUSTEXTBOOKID vm id = setUSXXID vm selectedUSLangIndex fVALUE1 id

getUSDICTONLINEID :: SettingsViewModel -> Maybe Int
getUSDICTONLINEID vm = getUSXXID vm selectedUSLangIndex fVALUE2
setUSDICTONLINEID :: SettingsViewModel -> Int -> SettingsViewModel
setUSDICTONLINEID vm id = setUSXXID vm selectedUSLangIndex fVALUE2 id

getUSDICTNOTEID :: SettingsViewModel -> Maybe Int
getUSDICTNOTEID vm = getUSXXID vm selectedUSLangIndex fVALUE3
setUSDICTNOTEID :: SettingsViewModel -> Int -> SettingsViewModel
setUSDICTNOTEID vm id = setUSXXID vm selectedUSLangIndex fVALUE3 id

getUSUNITFROM :: SettingsViewModel -> Maybe Int
getUSUNITFROM vm = getUSXXID vm selectedUSTextbookIndex fVALUE1
setUSUNITFROM :: SettingsViewModel -> Int -> SettingsViewModel
setUSUNITFROM vm id = setUSXXID vm selectedUSTextbookIndex fVALUE1 id

getUSPARTFROM :: SettingsViewModel -> Maybe Int
getUSPARTFROM vm = getUSXXID vm selectedUSTextbookIndex fVALUE2
setUSPARTFROM :: SettingsViewModel -> Int -> SettingsViewModel
setUSPARTFROM vm id = setUSXXID vm selectedUSTextbookIndex fVALUE2 id

getUSUNITTO :: SettingsViewModel -> Maybe Int
getUSUNITTO vm = getUSXXID vm selectedUSTextbookIndex fVALUE3
setUSUNITTO :: SettingsViewModel -> Int -> SettingsViewModel
setUSUNITTO vm id = setUSXXID vm selectedUSTextbookIndex fVALUE3 id

getUSPARTTO :: SettingsViewModel -> Maybe Int
getUSPARTTO vm = getUSXXID vm selectedUSTextbookIndex fVALUE4
setUSPARTTO :: SettingsViewModel -> Int -> SettingsViewModel
setUSPARTTO vm id = setUSXXID vm selectedUSTextbookIndex fVALUE4 id

getUSUNITPARTFROM :: SettingsViewModel -> Maybe Int
getUSUNITPARTFROM vm = (+) <$> (*10) <$> getUSUNITFROM vm <*> getUSPARTFROM vm

getUSUNITPARTTO :: SettingsViewModel -> Maybe Int
getUSUNITPARTTO vm = (+) <$> (*10) <$> getUSUNITTO vm <*> getUSPARTTO vm

isSingleUnitPart :: SettingsViewModel -> Maybe Bool
isSingleUnitPart vm = (==) <$> getUSUNITPARTFROM vm <*> getUSUNITPARTTO vm

isInvalidUnitPart :: SettingsViewModel -> Maybe Bool
isInvalidUnitPart vm = (>) <$> getUSUNITPARTFROM vm <*> getUSUNITPARTTO vm

