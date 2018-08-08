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
    mapM_ uprint $ vm ^. arrUserSettings
    print $ vm ^. selectedUSUserIndex
