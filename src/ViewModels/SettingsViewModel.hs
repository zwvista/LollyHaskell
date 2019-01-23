{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module ViewModels.SettingsViewModel
    ( SettingsViewModel
    , arrUserSettings
    , selectedUSUserIndex
    , selectedUSLangIndex
    , selectedUSTextbookIndex
    , arrLanguages
    , selectedLangIndex
    , arrDictsWord
    , arrDictsPicker
    , selectedDictPickerIndex
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
    , getUSDICTPICKER
    , setUSDICTPICKER
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
    , selectedDictPicker
    , selectedDictNote
    , selectedTextbook
    , ViewModels.SettingsViewModel.getData
    , setSelectedLangIndex
    , setSelectedTextbookIndex
    ) where

import Control.Concurrent.Async
import Control.Lens
import Control.Monad
import Data.Default.Class
import Data.List
import Data.Text (Text, splitOn)
import Data.Text.Read
import Formatting
import GHC.Generics (Generic)
import Models.MAutoCorrect
import Models.MDictPicker
import Models.MDictNote
import Models.MDictWord
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
    , _arrDictsWord :: [MDictWord]
    , _arrDictsPicker :: [MDictPicker]
    , _selectedDictPickerIndex :: Int
    , _arrDictsNote :: [MDictNote]
    , _selectedDictNoteIndex :: Int
    , _arrTextbooks :: [MTextbook]
    , _selectedTextbookIndex :: Int
    , _arrUnits :: [Text]
    , _arrParts :: [Text]
    , _arrAutoCorrect :: [MAutoCorrect]
    } deriving (Show, Generic, Default)
makeLenses ''SettingsViewModel

getUSXX :: Lens' SettingsViewModel Int -> Lens' MUserSetting (Maybe Text) -> SettingsViewModel -> Maybe Text
getUSXX fIndex fValue vm = vm ^. arrUserSettings ^?! ix (vm ^. fIndex) . fValue
setUSXX :: Lens' SettingsViewModel Int -> Lens' MUserSetting (Maybe Text) -> Text -> SettingsViewModel -> SettingsViewModel
setUSXX fIndex fValue id vm = vm & arrUserSettings . ix (vm ^. fIndex) . fValue . _Just .~ id

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

getUSDICTPICKER :: SettingsViewModel -> Text
getUSDICTPICKER vm = getUSXX selectedUSLangIndex fVALUE2 vm ^?! _Just
setUSDICTPICKER :: Text -> SettingsViewModel -> SettingsViewModel
setUSDICTPICKER id = setUSXX selectedUSLangIndex fVALUE2 id

getUSDICTNOTEID :: SettingsViewModel -> Int
getUSDICTNOTEID = getUSXXID selectedUSLangIndex fVALUE3
setUSDICTNOTEID :: Int -> SettingsViewModel -> SettingsViewModel
setUSDICTNOTEID id = setUSXXID selectedUSLangIndex fVALUE3 id

getUSDICTSPICKER :: SettingsViewModel -> Text
getUSDICTSPICKER vm = getUSXX selectedUSLangIndex fVALUE4 vm ^. non "0"
setUSDICTSPICKER :: Text -> SettingsViewModel -> SettingsViewModel
setUSDICTSPICKER id = setUSXX selectedUSLangIndex fVALUE4 id

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

selectedDictPicker :: SettingsViewModel -> MDictPicker
selectedDictPicker vm = vm ^. arrDictsPicker ^?! ix (vm ^. selectedDictPickerIndex)

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

computeDictPicker :: SettingsViewModel -> ([MDictPicker], Int) -> Text -> ([MDictPicker], Int)
computeDictPicker vm (lst, i) d
    | d == "0" = (lst ++ (vm ^. arrDictsWord <&> (\o -> MDictPicker{ _fDICTID = sformat int (o ^. Models.MDictWord.fDICTID), _fDICTNAME = o ^. Models.MDictWord.fDICTNAME })), i)
    | otherwise = (lst ++ [MDictPicker{ _fDICTID = d, _fDICTNAME = sformat ("Custom" % int) i }], i + 1)

setSelectedLangIndex :: Int -> SettingsViewModel -> IO SettingsViewModel
setSelectedLangIndex langindex vm = do
    let vm2 = vm & selectedLangIndex .~ langindex
        langid = selectedLang vm2 ^. Models.MLanguage.fID
        vm3 = vm2 & setUSLANGID langid
            & selectedUSLangIndex .~ (findIndex (\o -> o ^. fKIND == 2 && o ^. fENTITYID == langid) (vm2 ^.arrUserSettings) ^. non (-1))
        dicts = vm3 & getUSDICTSPICKER & splitOn "\r\n"
    (r1, r2, r3, r4)
        <- runConcurrently $ (,,,)
        <$> (Concurrently (Models.MDictWord.getDataByLang langid))
        <*> (Concurrently (Models.MDictNote.getDataByLang langid))
        <*> (Concurrently (Models.MTextbook.getDataByLang langid))
        <*> (Concurrently (Models.MAutoCorrect.getDataByLang langid))
    let vm4 = vm3 & arrDictsWord .~ r1
        vm5 = vm4
            & arrDictsPicker .~ (foldl (computeDictPicker vm4) ([], 1) dicts ^. _1)
            & selectedDictPickerIndex .~ (findIndex (\o -> o ^. Models.MDictPicker.fDICTID == getUSDICTPICKER vm4) (vm4 ^. arrDictsPicker) ^. non (-1))
            & arrDictsNote .~ r2
            & selectedDictNoteIndex .~ (findIndex (\o -> o ^. Models.MDictNote.fID == getUSDICTNOTEID vm4) r2 ^. non (-1))
            & arrTextbooks .~ r3
            & arrAutoCorrect .~ r4
    return $ setSelectedTextbookIndex (findIndex (\o -> o ^. Models.MTextbook.fID == getUSTEXTBOOKID vm4) r3 ^. non (-1)) vm4

setSelectedTextbookIndex :: Int -> SettingsViewModel -> SettingsViewModel
setSelectedTextbookIndex textbookindex vm =
    let vm2 = vm & selectedTextbookIndex .~ textbookindex
        textbookid = selectedTextbook vm2 ^. Models.MTextbook.fID
    in vm2 & setUSTEXTBOOKID textbookid
        & selectedUSTextbookIndex .~ (findIndex (\o -> o ^. fKIND == 3 && o ^. fENTITYID == textbookid) (vm2 ^.arrUserSettings) ^. non (-1))
        & arrUnits .~ ([1..(selectedTextbook vm2 ^. fUNITS)] <&> sformat int)
        & arrParts .~ (selectedTextbook vm2 ^. fPARTS & splitOn " ")
