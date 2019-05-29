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
    , arrDictsGroup
    , selectedDictGroupIndex
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
    , getUSDICTGROUP
    , setUSDICTGROUP
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
    , selectedDictGroup
    , selectedDictNote
    , selectedTextbook
    , ViewModels.SettingsViewModel.getData
    , setSelectedLangIndex
    , setSelectedTextbookIndex
    ) where

import Control.Concurrent.Async
import Control.Lens
import Data.Default.Class
import Data.List
import Data.Text (Text, splitOn)
import Data.Text.Read
import Formatting
import GHC.Generics (Generic)
import Models.MAutoCorrect
import Models.MDictGroup
import Models.MDictNote
import Models.MDictMean
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
    , _arrDictsWord :: [MDictMean]
    , _arrDictsGroup :: [MDictGroup]
    , _selectedDictGroupIndex :: Int
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
setUSXX fIndex fValue fid vm = vm & arrUserSettings . ix (vm ^. fIndex) . fValue . _Just .~ fid

getUSXXID :: Lens' SettingsViewModel Int -> Lens' MUserSetting (Maybe Text) -> SettingsViewModel -> Int
getUSXXID fIndex fValue vm = vm ^. arrUserSettings ^?! ix (vm ^. fIndex) . fValue ^?! _Just . to decimal ^?! _Right . _1
setUSXXID :: Lens' SettingsViewModel Int -> Lens' MUserSetting (Maybe Text) -> Int -> SettingsViewModel -> SettingsViewModel
setUSXXID fIndex fValue fid vm = vm & arrUserSettings . ix (vm ^. fIndex) . fValue . _Just .~ sformat int fid

getUSLANGID :: SettingsViewModel -> Int
getUSLANGID = getUSXXID selectedUSUserIndex fVALUE1
setUSLANGID :: Int -> SettingsViewModel -> SettingsViewModel
setUSLANGID = setUSXXID selectedUSUserIndex fVALUE1

getUSTEXTBOOKID :: SettingsViewModel -> Int
getUSTEXTBOOKID = getUSXXID selectedUSLangIndex fVALUE1
setUSTEXTBOOKID :: Int -> SettingsViewModel -> SettingsViewModel
setUSTEXTBOOKID = setUSXXID selectedUSLangIndex fVALUE1

getUSDICTGROUP :: SettingsViewModel -> Text
getUSDICTGROUP vm = getUSXX selectedUSLangIndex fVALUE2 vm ^?! _Just
setUSDICTGROUP :: Text -> SettingsViewModel -> SettingsViewModel
setUSDICTGROUP = setUSXX selectedUSLangIndex fVALUE2

getUSDICTNOTEID :: SettingsViewModel -> Int
getUSDICTNOTEID = getUSXXID selectedUSLangIndex fVALUE3
setUSDICTNOTEID :: Int -> SettingsViewModel -> SettingsViewModel
setUSDICTNOTEID = setUSXXID selectedUSLangIndex fVALUE3

getUSDICTSGROUP :: SettingsViewModel -> Text
getUSDICTSGROUP vm = getUSXX selectedUSLangIndex fVALUE4 vm ^. non "0"
setUSDICTSGROUP :: Text -> SettingsViewModel -> SettingsViewModel
setUSDICTSGROUP = setUSXX selectedUSLangIndex fVALUE4

getUSUNITFROM :: SettingsViewModel -> Int
getUSUNITFROM = getUSXXID selectedUSTextbookIndex fVALUE1
setUSUNITFROM :: Int -> SettingsViewModel -> SettingsViewModel
setUSUNITFROM = setUSXXID selectedUSTextbookIndex fVALUE1

getUSPARTFROM :: SettingsViewModel -> Int
getUSPARTFROM = getUSXXID selectedUSTextbookIndex fVALUE2
setUSPARTFROM :: Int -> SettingsViewModel -> SettingsViewModel
setUSPARTFROM = setUSXXID selectedUSTextbookIndex fVALUE2

getUSUNITTO :: SettingsViewModel -> Int
getUSUNITTO = getUSXXID selectedUSTextbookIndex fVALUE3
setUSUNITTO :: Int -> SettingsViewModel -> SettingsViewModel
setUSUNITTO = setUSXXID selectedUSTextbookIndex fVALUE3

getUSPARTTO :: SettingsViewModel -> Int
getUSPARTTO = getUSXXID selectedUSTextbookIndex fVALUE4
setUSPARTTO :: Int -> SettingsViewModel -> SettingsViewModel
setUSPARTTO = setUSXXID selectedUSTextbookIndex fVALUE4

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

selectedDictGroup :: SettingsViewModel -> MDictGroup
selectedDictGroup vm = vm ^. arrDictsGroup ^?! ix (vm ^. selectedDictGroupIndex)

selectedDictNote :: SettingsViewModel -> MDictNote
selectedDictNote vm = vm ^. arrDictsNote ^?! ix (vm ^. selectedDictNoteIndex)

selectedTextbook :: SettingsViewModel -> MTextbook
selectedTextbook vm = vm ^. arrTextbooks ^?! ix (vm ^. selectedTextbookIndex)

getData :: IO SettingsViewModel
getData = do
    (r1, r2) <- concurrently Models.MLanguage.getData (Models.MUserSetting.getDataByUser 1)
    let vm =
            (def :: SettingsViewModel) & arrLanguages .~ r1 & arrUserSettings .~ r2 &
            selectedUSUserIndex .~ (findIndex (\o -> o ^. fKIND == 1) r2 ^. non (-1))
    setSelectedLangIndex (findIndex (\o -> o ^. Models.MLanguage.fID == getUSLANGID vm) r1 ^. non (-1)) vm

computeDictGroup :: [MDictMean] -> ([MDictGroup], Int) -> Text -> ([MDictGroup], Int)
computeDictGroup arr (lst, i) d
    | d == "0" = (lst ++ (arr <&> (\o -> MDictGroup{ _fDICTID = sformat int (o ^. Models.MDictMean.fDICTID), _fDICTNAME = o ^. Models.MDictMean.fDICTNAME })), i)
    | otherwise = (lst ++ [MDictGroup{ _fDICTID = d, _fDICTNAME = sformat ("Custom" % int) i }], i + 1)

setSelectedLangIndex :: Int -> SettingsViewModel -> IO SettingsViewModel
setSelectedLangIndex langindex vm = do
    let vm2 = vm & selectedLangIndex .~ langindex
        langid = selectedLang vm2 ^. Models.MLanguage.fID
        vm3 =
            vm2 & setUSLANGID langid &
            selectedUSLangIndex .~
            (findIndex (\o -> o ^. fKIND == 2 && o ^. fENTITYID == langid) (vm2 ^. arrUserSettings) ^. non (-1))
        dicts = vm3 & getUSDICTSGROUP & splitOn "\r\n"
    (r1, r2, r3, r4) <-
        runConcurrently $ (,,,) <$>
        Concurrently (Models.MDictMean.getDataByLang langid) <*>
        Concurrently (Models.MDictNote.getDataByLang langid) <*>
        Concurrently (Models.MTextbook.getDataByLang langid) <*>
        Concurrently (Models.MAutoCorrect.getDataByLang langid)
    let vm4 = vm3 & arrDictsWord .~ r1 & arrDictsGroup .~ (foldl (computeDictGroup r1) ([], 1) dicts ^. _1)
        vm5 =
            vm4 &
            selectedDictGroupIndex .~
            (findIndex (\o -> o ^. Models.MDictGroup.fDICTID == getUSDICTGROUP vm4) (vm4 ^. arrDictsGroup) ^. non (-1)) &
            arrDictsNote .~ r2 &
            selectedDictNoteIndex .~ (findIndex (\o -> o ^. Models.MDictNote.fID == getUSDICTNOTEID vm4) r2 ^. non (-1)) &
            arrTextbooks .~ r3 &
            arrAutoCorrect .~ r4
    return $
        setSelectedTextbookIndex (findIndex (\o -> o ^. Models.MTextbook.fID == getUSTEXTBOOKID vm5) r3 ^. non (-1)) vm5

setSelectedTextbookIndex :: Int -> SettingsViewModel -> SettingsViewModel
setSelectedTextbookIndex textbookindex vm =
    let vm2 = vm & selectedTextbookIndex .~ textbookindex
        textbookid = selectedTextbook vm2 ^. Models.MTextbook.fID
    in vm2 & setUSTEXTBOOKID textbookid
        & selectedUSTextbookIndex .~ (findIndex (\o -> o ^. fKIND == 3 && o ^. fENTITYID == textbookid) (vm2 ^.arrUserSettings) ^. non (-1))
        & arrUnits .~ ([1..(selectedTextbook vm2 ^. fUNITS)] <&> sformat int)
        & arrParts .~ (selectedTextbook vm2 ^. fPARTS & splitOn " ")
