module Lib
    ( someFunc
    ) where

import Control.Lens
import SettingsViewModel
import Text.Show.Unicode

someFunc :: IO ()
someFunc = do
    vm <- SettingsViewModel.getData
    mapM_ uprint $ vm ^. arrLanguages
    uprint $ vm ^. arrUserSettings ^?! ix (vm ^. selectedUSUserIndex)
    uprint $ vm ^. arrUserSettings ^?! ix (vm ^. selectedUSLangIndex)
    uprint $ vm ^. arrUserSettings ^?! ix (vm ^. selectedUSTextbookIndex)
    print $ vm ^. arrUnits & length
