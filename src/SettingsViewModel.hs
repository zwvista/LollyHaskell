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
    , arrUnits
    , arrParts
    , getUSLANGID
    , setUSLANGID
    , getUSTEXTBOOKID
    , setUSTEXTBOOKID
    , getUSDICTONLINEID
    , setUSDICTONLINEID
    , getUSDICTNOTEID
    , setUSDICTNOTEID
    , getUSUNITFROM
    , setUSUNITFROM
    , getUSPARTFROM
    , setUSPARTFROM
    , getUSUNITTO
    , setUSUNITTO
    , getUSPARTTO
    , setUSPARTTO
    , getUSUNITPARTFROM
    , getUSUNITPARTTO
    , isSingleUnitPart
    , isInvalidUnitPart
    , selectedLang
    , selectedDictOnline
    , selectedDictNote
    , selectedTextbook
    , SettingsViewModel.getData
    , setSelectedLangIndex
    , setSelectedTextbookIndex
    ) where

import Control.Concurrent.Async
import Control.Lens
import Control.Monad
import Control.Arrow
import Data.Default.Class
import Data.List
import Data.Text (Text, splitOn)
import Data.Text.Read
import Formatting
import GHC.Generics (Generic)
import Models.MDictNote
import Models.MDictOnline
import Models.MLanguage
import Models.MTextbook
import Models.MUserSetting

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
    , _arrUnits :: [Text]
    , _arrParts :: [Text]
    } deriving (Show, Generic, Default)
makeLenses ''SettingsViewModel

getUSXXID :: Lens' SettingsViewModel Int -> Lens' MUserSetting (Maybe Text) -> SettingsViewModel -> Int
getUSXXID fIndex fValue vm = vm ^. arrUserSettings ^?! ix (vm ^. fIndex) . fValue ^?! _Just . to decimal ^?! _Right . _1
setUSXXID :: Lens' SettingsViewModel Int -> Lens' MUserSetting (Maybe Text) -> Int -> SettingsViewModel -> SettingsViewModel
setUSXXID fIndex fValue id vm = vm & arrUserSettings . ix (vm ^. fIndex) . fValue . _Just .~ (sformat int id)

getUSLANGID :: SettingsViewModel -> Int
getUSLANGID = getUSXXID selectedUSUserIndex fVALUE1
setUSLANGID :: Int -> SettingsViewModel -> SettingsViewModel
setUSLANGID id = setUSXXID selectedUSUserIndex fVALUE1 id

getUSTEXTBOOKID :: SettingsViewModel -> Int
getUSTEXTBOOKID = getUSXXID selectedUSLangIndex fVALUE1
setUSTEXTBOOKID :: Int -> SettingsViewModel -> SettingsViewModel
setUSTEXTBOOKID id = setUSXXID selectedUSLangIndex fVALUE1 id

getUSDICTONLINEID :: SettingsViewModel -> Int
getUSDICTONLINEID = getUSXXID selectedUSLangIndex fVALUE2
setUSDICTONLINEID :: Int -> SettingsViewModel -> SettingsViewModel
setUSDICTONLINEID id = setUSXXID selectedUSLangIndex fVALUE2 id

getUSDICTNOTEID :: SettingsViewModel -> Int
getUSDICTNOTEID = getUSXXID selectedUSLangIndex fVALUE3
setUSDICTNOTEID :: Int -> SettingsViewModel -> SettingsViewModel
setUSDICTNOTEID id = setUSXXID selectedUSLangIndex fVALUE3 id

getUSUNITFROM :: SettingsViewModel -> Int
getUSUNITFROM = getUSXXID selectedUSTextbookIndex fVALUE1
setUSUNITFROM :: Int -> SettingsViewModel -> SettingsViewModel
setUSUNITFROM id = setUSXXID selectedUSTextbookIndex fVALUE1 id

getUSPARTFROM :: SettingsViewModel -> Int
getUSPARTFROM = getUSXXID selectedUSTextbookIndex fVALUE2
setUSPARTFROM :: Int -> SettingsViewModel -> SettingsViewModel
setUSPARTFROM id = setUSXXID selectedUSTextbookIndex fVALUE2 id

getUSUNITTO :: SettingsViewModel -> Int
getUSUNITTO = getUSXXID selectedUSTextbookIndex fVALUE3
setUSUNITTO :: Int -> SettingsViewModel -> SettingsViewModel
setUSUNITTO id = setUSXXID selectedUSTextbookIndex fVALUE3 id

getUSPARTTO :: SettingsViewModel -> Int
getUSPARTTO = getUSXXID selectedUSTextbookIndex fVALUE4
setUSPARTTO :: Int -> SettingsViewModel -> SettingsViewModel
setUSPARTTO id = setUSXXID selectedUSTextbookIndex fVALUE4 id

getUSUNITPARTFROM :: SettingsViewModel -> Int
getUSUNITPARTFROM vm = getUSUNITFROM vm * 10 + getUSPARTFROM vm

getUSUNITPARTTO :: SettingsViewModel -> Int
getUSUNITPARTTO vm = getUSUNITTO vm * 10 + getUSPARTTO vm

isSingleUnitPart :: SettingsViewModel -> Bool
isSingleUnitPart vm = getUSUNITPARTFROM vm == getUSUNITPARTTO vm

isInvalidUnitPart :: SettingsViewModel -> Bool
isInvalidUnitPart vm = getUSUNITPARTFROM vm > getUSUNITPARTTO vm

selectedLang :: SettingsViewModel -> MLanguage
selectedLang vm = vm ^. arrLanguages ^?! ix (vm ^. selectedLangIndex)

selectedDictOnline :: SettingsViewModel -> MDictOnline
selectedDictOnline vm = vm ^. arrDictsOnline ^?! ix (vm ^. selectedDictOnlineIndex)

selectedDictNote :: SettingsViewModel -> MDictNote
selectedDictNote vm = vm ^. arrDictsNote ^?! ix (vm ^. selectedDictNoteIndex)

selectedTextbook :: SettingsViewModel -> MTextbook
selectedTextbook vm = vm ^. arrTextbooks ^?! ix (vm ^. selectedTextbookIndex)

getData :: IO SettingsViewModel
getData = do
    (r1, r2) <- concurrently (Models.MLanguage.getData) (Models.MUserSetting.getDataByUser 1)
    let vm = (def :: SettingsViewModel)
            & arrLanguages .~ r1
            & arrUserSettings .~ r2
            & selectedUSUserIndex .~ (findIndex (\o -> o ^. fKIND == 1) r2 ^. non (-1))
    setSelectedLangIndex (findIndex (\o -> o ^. Models.MLanguage.fID == getUSLANGID vm) r1 ^. non (-1)) vm

setSelectedLangIndex :: Int -> SettingsViewModel -> IO SettingsViewModel
setSelectedLangIndex langindex vm = do
    let vm2 = vm & selectedLangIndex .~ langindex
        langid = selectedLang vm2 ^. Models.MLanguage.fID
        vm3 = vm2 & setUSLANGID langid
            & selectedUSLangIndex .~ (findIndex (\o -> o ^. fKIND == 2 && o ^. fENTITYID == langid) (vm2 ^.arrUserSettings) ^. non (-1))
    (r1, r2, r3)
        <- runConcurrently $ (,,)
        <$> (Concurrently (Models.MDictOnline.getDataByLang langid))
        <*> (Concurrently (Models.MDictNote.getDataByLang langid))
        <*> (Concurrently (Models.MTextbook.getDataByLang langid))
    let vm4 = vm3
            & arrDictsOnline .~ r1
            & selectedDictOnlineIndex .~ (findIndex (\o -> o ^. Models.MDictOnline.fID == getUSDICTONLINEID vm3) r1 ^. non (-1))
            & arrDictsNote .~ r2
            & selectedDictNoteIndex .~ (findIndex (\o -> o ^. Models.MDictNote.fID == getUSDICTNOTEID vm3) r2 ^. non (-1))
            & arrTextbooks .~ r3
    return $ setSelectedTextbookIndex (findIndex (\o -> o ^. Models.MTextbook.fID == getUSTEXTBOOKID vm3) r3 ^. non (-1)) vm4

setSelectedTextbookIndex :: Int -> SettingsViewModel -> SettingsViewModel
setSelectedTextbookIndex textbookindex vm =
    let vm2 = vm & selectedTextbookIndex .~ textbookindex
        textbookid = selectedTextbook vm2 ^. Models.MTextbook.fID
    in vm2 & setUSTEXTBOOKID textbookid
        & selectedUSTextbookIndex .~ (findIndex (\o -> o ^. fKIND == 3 && o ^. fENTITYID == textbookid) (vm2 ^.arrUserSettings) ^. non (-1))
        & arrUnits .~ ([1..(selectedTextbook vm2 ^. fUNITS)] <&> sformat int)
        & arrParts .~ (selectedTextbook vm2 ^. fPARTS & splitOn " ")
