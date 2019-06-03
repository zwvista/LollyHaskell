{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module ViewModels.SettingsViewModel
    ( SettingsViewModel
    , arrUserSettings
    , selectedUSUser0
    , selectedUSUser1
    , fUSLEVELCOLORS
    , selectedUSLang2
    , selectedUSLang3
    , selectedUSLang4
    , selectedUSTextbook
    , arrLanguages
    , selectedLang
    , setSelectedLang
    , voices
    , selectedVoice
    , setSelectedVoice
    , arrDictsReference
    , arrDictItems
    , selectedDictItem
    , setSelectedDictItem
    , arrDictsNote
    , selectedDictNote
    , setSelectedDictNote
    , arrDictsTranslation
    , selectedDictTranslation
    , setSelectedDictTranslation
    , arrTextbooks
    , selectedTextbook
    , setSelectedTextbook
    , arrUnits
    , getUnitCount
    , arrParts
    , getPartCount
    , arrAutoCorrect
    , getUSLANGID
    , setUSLANGID
    , getUSROWSPERPAGEOPTIONS
    , getUSROWSPERPAGE
    , getUSREADINTERVAL
    , getUSREVIEWINTERVAL
    , getUSDICTITEM
    , setUSDICTITEM
    , getUSTEXTBOOKID
    , setUSTEXTBOOKID
    , getUSDICTNOTEID
    , setUSDICTNOTEID
    , getUSDICTITEMS
    , setUSDICTITEMS
    , getUSDICTTRANSLATIONID
    , setUSDICTTRANSLATIONID
    , getUSVOICEID
    , setUSVOICEID
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
    , isSingleUnit
    , isInvalidUnitPart
    , ViewModels.SettingsViewModel.getData
    ) where

import Control.Concurrent.Async
import Control.Lens
import Data.Default.Class
import Data.List
import Data.Map (Map, fromList)
import Data.Text (Text, splitOn)
import Data.Text.Read
import Formatting
import GHC.Generics (Generic)
import Models.MAutoCorrect
import Models.MDictItem
import Models.MDictNote
import Models.MDictReference
import Models.MDictTranslation
import Models.MLanguage
import Models.MTextbook
import Models.MUserSetting
import Models.MVoice

data SettingsViewModel = SettingsViewModel
    { _arrUserSettings :: [MUserSetting]
    , _selectedUSUser0 :: MUserSetting
    , _selectedUSUser1 :: MUserSetting
    , _fUSLEVELCOLORS :: Map Text [Text]
    , _selectedUSLang2 :: MUserSetting
    , _selectedUSLang3 :: MUserSetting
    , _selectedUSLang4 :: MUserSetting
    , _selectedUSTextbook :: MUserSetting
    , _arrLanguages :: [MLanguage]
    , _selectedLang :: MLanguage
    , _voices :: [MVoice]
    , _selectedVoice :: Maybe MVoice
    , _arrDictsReference :: [MDictReference]
    , _arrDictItems :: [MDictItem]
    , _selectedDictItem :: MDictItem
    , _arrDictsNote :: [MDictNote]
    , _selectedDictNote :: Maybe MDictNote
    , _arrDictsTranslation :: [MDictTranslation]
    , _selectedDictTranslation :: Maybe MDictTranslation
    , _arrTextbooks :: [MTextbook]
    , _selectedTextbook :: MTextbook
    , _arrUnits :: [Text]
    , _arrParts :: [Text]
    , _arrAutoCorrect :: [MAutoCorrect]
    } deriving (Show, Generic, Default)
makeLenses ''SettingsViewModel

getUSXX :: Lens' SettingsViewModel MUserSetting -> Lens' MUserSetting (Maybe Text) -> SettingsViewModel -> Maybe Text
getUSXX fUserSetting fValue vm = vm ^. fUserSetting . fValue
setUSXX :: Lens' SettingsViewModel MUserSetting -> Lens' MUserSetting (Maybe Text) -> Text -> SettingsViewModel -> SettingsViewModel
setUSXX fUserSetting fValue vtext vm = vm & fUserSetting . fValue . _Just .~ vtext

getUSXXID :: Lens' SettingsViewModel MUserSetting -> Lens' MUserSetting (Maybe Text) -> SettingsViewModel -> Maybe Int
getUSXXID fUserSetting fValue vm = vm ^. fUserSetting . fValue <&> decimal <&> (^?! _Right . _1)
setUSXXID :: Lens' SettingsViewModel MUserSetting -> Lens' MUserSetting (Maybe Text) -> Int -> SettingsViewModel -> SettingsViewModel
setUSXXID fUserSetting fValue vid vm = vm & fUserSetting . fValue . _Just .~ sformat int vid

getUSLANGID :: SettingsViewModel -> Int
getUSLANGID vm = getUSXXID selectedUSUser0 fVALUE1 vm ^?! _Just
setUSLANGID :: Int -> SettingsViewModel -> SettingsViewModel
setUSLANGID = setUSXXID selectedUSUser0 fVALUE1

getUSROWSPERPAGEOPTIONS :: SettingsViewModel -> Maybe Text
getUSROWSPERPAGEOPTIONS = getUSXX selectedUSUser0 fVALUE2

getUSROWSPERPAGE :: SettingsViewModel -> Maybe Int
getUSROWSPERPAGE = getUSXXID selectedUSUser0 fVALUE3

getUSREADINTERVAL :: SettingsViewModel -> Maybe Int
getUSREADINTERVAL = getUSXXID selectedUSUser1 fVALUE1

getUSREVIEWINTERVAL :: SettingsViewModel -> Maybe Int
getUSREVIEWINTERVAL = getUSXXID selectedUSUser1 fVALUE2

getUSTEXTBOOKID :: SettingsViewModel -> Int
getUSTEXTBOOKID vm = getUSXXID selectedUSLang2 fVALUE1 vm ^?! _Just
setUSTEXTBOOKID :: Int -> SettingsViewModel -> SettingsViewModel
setUSTEXTBOOKID = setUSXXID selectedUSLang2 fVALUE1

getUSDICTITEM :: SettingsViewModel -> Text
getUSDICTITEM vm = getUSXX selectedUSLang2 fVALUE2 vm ^?! _Just
setUSDICTITEM :: Text -> SettingsViewModel -> SettingsViewModel
setUSDICTITEM = setUSXX selectedUSLang2 fVALUE2

getUSDICTNOTEID :: SettingsViewModel -> Int
getUSDICTNOTEID vm = getUSXXID selectedUSLang2 fVALUE3 vm ^. non 0
setUSDICTNOTEID :: Int -> SettingsViewModel -> SettingsViewModel
setUSDICTNOTEID = setUSXXID selectedUSLang2 fVALUE3

getUSDICTITEMS :: SettingsViewModel -> Text
getUSDICTITEMS vm = getUSXX selectedUSLang2 fVALUE4 vm ^. non "0"
setUSDICTITEMS :: Text -> SettingsViewModel -> SettingsViewModel
setUSDICTITEMS = setUSXX selectedUSLang2 fVALUE4

getUSDICTTRANSLATIONID :: SettingsViewModel -> Int
getUSDICTTRANSLATIONID vm = getUSXXID selectedUSLang3 fVALUE1 vm ^. non 0
setUSDICTTRANSLATIONID :: Int -> SettingsViewModel -> SettingsViewModel
setUSDICTTRANSLATIONID = setUSXXID selectedUSLang3 fVALUE1

getUSVOICEID :: SettingsViewModel -> Int
getUSVOICEID vm = getUSXXID selectedUSLang4 fVALUE1 vm ^. non 0
setUSVOICEID :: Int -> SettingsViewModel -> SettingsViewModel
setUSVOICEID = setUSXXID selectedUSLang4 fVALUE1

getUSUNITFROM :: SettingsViewModel -> Int
getUSUNITFROM vm = getUSXXID selectedUSTextbook fVALUE1 vm ^. non 1
setUSUNITFROM :: Int -> SettingsViewModel -> SettingsViewModel
setUSUNITFROM = setUSXXID selectedUSTextbook fVALUE1

getUSPARTFROM :: SettingsViewModel -> Int
getUSPARTFROM vm = getUSXXID selectedUSTextbook fVALUE2 vm ^. non 1
setUSPARTFROM :: Int -> SettingsViewModel -> SettingsViewModel
setUSPARTFROM = setUSXXID selectedUSTextbook fVALUE2

getUSUNITTO :: SettingsViewModel -> Int
getUSUNITTO vm = getUSXXID selectedUSTextbook fVALUE3 vm ^. non 1
setUSUNITTO :: Int -> SettingsViewModel -> SettingsViewModel
setUSUNITTO = setUSXXID selectedUSTextbook fVALUE3

getUSPARTTO :: SettingsViewModel -> Int
getUSPARTTO vm = getUSXXID selectedUSTextbook fVALUE4 vm ^. non 1
setUSPARTTO :: Int -> SettingsViewModel -> SettingsViewModel
setUSPARTTO = setUSXXID selectedUSTextbook fVALUE4

getUSUNITPARTFROM :: SettingsViewModel -> Int
getUSUNITPARTFROM vm = getUSUNITFROM vm * 10 + getUSPARTFROM vm

getUSUNITPARTTO :: SettingsViewModel -> Int
getUSUNITPARTTO vm = getUSUNITTO vm * 10 + getUSPARTTO vm

isSingleUnitPart :: SettingsViewModel -> Bool
isSingleUnitPart vm = getUSUNITPARTFROM vm == getUSUNITPARTTO vm

isSingleUnit :: SettingsViewModel -> Bool
isSingleUnit vm = getUSUNITFROM vm == getUSUNITTO vm && getUSPARTFROM vm == 1 && getUSPARTTO vm == getPartCount vm

isInvalidUnitPart :: SettingsViewModel -> Bool
isInvalidUnitPart vm = getUSUNITPARTFROM vm > getUSUNITPARTTO vm

setSelectedVoice :: MVoice -> SettingsViewModel -> SettingsViewModel
setSelectedVoice v vm = vm & selectedVoice . _Just .~ v
    & setUSVOICEID (v ^. Models.MVoice.fID)

setSelectedDictItem :: MDictItem -> SettingsViewModel -> SettingsViewModel
setSelectedDictItem v vm = vm & selectedDictItem .~ v
    & setUSDICTITEM (v ^. Models.MDictItem.fDICTID)

setSelectedDictNote :: MDictNote -> SettingsViewModel -> SettingsViewModel
setSelectedDictNote v vm = vm & selectedDictNote . _Just .~ v
    & setUSDICTNOTEID (v ^. Models.MDictNote.fID)

setSelectedDictTranslation :: MDictTranslation -> SettingsViewModel -> SettingsViewModel
setSelectedDictTranslation v vm = vm & selectedDictTranslation . _Just .~ v
    & setUSDICTTRANSLATIONID (v ^. Models.MDictTranslation.fID)

setSelectedTextbook :: MTextbook -> SettingsViewModel -> SettingsViewModel
setSelectedTextbook v vm =
    let textbookid = v ^. Models.MTextbook.fID
    in vm & selectedTextbook .~ v & setUSTEXTBOOKID textbookid
        & selectedUSTextbook .~ find (\o -> o ^. fKIND == 11 && o ^. fENTITYID == textbookid) (vm ^.arrUserSettings) ^?! _Just

getUnitCount :: SettingsViewModel -> Int
getUnitCount vm = vm ^. arrUnits & length

getPartCount :: SettingsViewModel -> Int
getPartCount vm = vm ^. arrParts & length

getData :: IO SettingsViewModel
getData = do
    (r1, r2) <- concurrently Models.MLanguage.getData (Models.MUserSetting.getDataByUser 1)
    let vm = (def :: SettingsViewModel) & arrLanguages .~ r1 & arrUserSettings .~ r2
            & selectedUSUser0 .~ find (\o -> o ^. fKIND == 1 && o ^. fENTITYID == 0) r2 ^?! _Just
            & selectedUSUser1 .~ find (\o -> o ^. fKIND == 1 && o ^. fENTITYID == 1) r2 ^?! _Just
        arr = vm ^. selectedUSUser0 . fVALUE4 ^?! _Just & splitOn "\r\n" <&> splitOn "," <&> (\x -> (head x, tail x))
        vm2 = vm & fUSLEVELCOLORS .~ fromList arr
    setSelectedLang (find (\o -> o ^. Models.MLanguage.fID == getUSLANGID vm2) r1 ^?! _Just) vm2

computeDictItem :: [MDictReference] -> ([MDictItem], Int) -> Text -> ([MDictItem], Int)
computeDictItem arr (lst, i) d
    | d == "0" = (lst ++ (arr <&> (\o -> MDictItem{ _fDICTID = sformat int (o ^. Models.MDictReference.fDICTID), _fDICTNAME = o ^. Models.MDictReference.fDICTNAME })), i)
    | otherwise = (lst ++ [MDictItem{ _fDICTID = d, _fDICTNAME = sformat ("Custom" % int) i }], i + 1)

setSelectedLang :: MLanguage -> SettingsViewModel -> IO SettingsViewModel
setSelectedLang lang vm = do
    let langid = lang ^. Models.MLanguage.fID
        vm2 = vm & selectedLang .~ lang & setUSLANGID langid
            & selectedUSLang2 .~ find (\o -> o ^. fKIND == 2 && o ^. fENTITYID == langid) (vm ^. arrUserSettings) ^?! _Just
            & selectedUSLang3 .~ find (\o -> o ^. fKIND == 3 && o ^. fENTITYID == langid) (vm ^. arrUserSettings) ^?! _Just
            & selectedUSLang4 .~ find (\o -> o ^. fKIND == 4 && o ^. fENTITYID == langid) (vm ^. arrUserSettings) ^?! _Just
        dicts = vm2 & getUSDICTITEMS & splitOn "\r\n"
    (r1, r2, r3, r4, r5) <-
        runConcurrently $ (,,,,) <$>
        Concurrently (Models.MDictReference.getDataByLang langid) <*>
        Concurrently (Models.MDictNote.getDataByLang langid) <*>
        Concurrently (Models.MDictTranslation.getDataByLang langid) <*>
        Concurrently (Models.MTextbook.getDataByLang langid) <*>
        Concurrently (Models.MAutoCorrect.getDataByLang langid)
    let vm3 = vm2 & arrDictsReference .~ r1
            & arrDictItems .~ (foldl (computeDictItem r1) ([], 1) dicts ^. _1)
            & arrDictsNote .~ r2 & arrDictsTranslation .~ r3 & arrTextbooks .~ r4 & arrAutoCorrect .~ r5
        vm4 = vm3
            & selectedDictItem .~ find (\o -> o ^. Models.MDictItem.fDICTID == getUSDICTITEMS vm3) (vm3 ^. arrDictItems) ^?! _Just
            & selectedDictNote .~ find (\o -> o ^. Models.MDictNote.fID == getUSDICTNOTEID vm3) r2
            & selectedDictTranslation .~ find (\o -> o ^. Models.MDictTranslation.fID == getUSDICTTRANSLATIONID vm3) r2
    return $
        setSelectedTextbook (find (\o -> o ^. Models.MTextbook.fID == getUSTEXTBOOKID vm4) r4 ^?! _Just) vm4
