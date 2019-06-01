module Lib
    ( someFunc
    ) where

import Control.Lens
import Data.List (find)
import Models.MDictItem
import Models.MDictReference
import Text.Show.Unicode
import ViewModels.SettingsViewModel

someFunc :: IO ()
someFunc = do
    vm <- getData
    return ()
--    uprint $ vm ^. arrUserSettings ^?! ix (vm ^. selectedUSUserIndex)
--    uprint $ vm ^. arrUserSettings ^?! ix (vm ^. selectedUSLangIndex)
--    uprint $ vm ^. arrUserSettings ^?! ix (vm ^. selectedUSTextbookIndex)
--    uprint $ selectedLang vm
--    uprint $ selectedDictItem vm
--    uprint $ find (\o -> o ^. Models.MDictReference.fDICTNAME == selectedDictItem vm ^. Models.MDictItem.fDICTNAME) (vm ^. arrDictsWord) ^?! _Just
--    uprint $ selectedDictNote vm
--    uprint $ selectedTextbook vm
