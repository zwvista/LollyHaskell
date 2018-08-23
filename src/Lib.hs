module Lib
    ( someFunc
    ) where

import Control.Lens
import SettingsViewModel
import Text.Show.Unicode

someFunc :: IO ()
someFunc = do
    vm <- SettingsViewModel.getData
    uprint $ vm ^. arrUserSettings ^?! ix (vm ^. selectedUSUserIndex)
    uprint $ vm ^. arrUserSettings ^?! ix (vm ^. selectedUSLangIndex)
    uprint $ vm ^. arrUserSettings ^?! ix (vm ^. selectedUSTextbookIndex)
    uprint $ selectedLang vm
    uprint $ selectedDictOnline vm
    uprint $ selectedDictNote vm
    uprint $ selectedTextbook vm
