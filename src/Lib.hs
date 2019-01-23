module Lib
    ( someFunc
    ) where

import Control.Lens
import Data.List (find)
import Models.MDictPicker
import Models.MDictWord
import Text.Show.Unicode
import ViewModels.SettingsViewModel

someFunc :: IO ()
someFunc = do
    vm <- getData
    uprint $ vm ^. arrUserSettings ^?! ix (vm ^. selectedUSUserIndex)
    uprint $ vm ^. arrUserSettings ^?! ix (vm ^. selectedUSLangIndex)
    uprint $ vm ^. arrUserSettings ^?! ix (vm ^. selectedUSTextbookIndex)
    uprint $ selectedLang vm
    uprint $ selectedDictPicker vm
    uprint $ find (\o -> o ^. Models.MDictWord.fDICTNAME == selectedDictPicker vm ^. Models.MDictPicker.fDICTNAME) (vm ^. arrDictsWord) ^?! _Just
    uprint $ selectedDictNote vm
    uprint $ selectedTextbook vm
