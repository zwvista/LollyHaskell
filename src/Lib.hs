module Lib
    ( someFunc
    ) where

import Models.MDictionary
import Models.MLangPhrase
import Models.MLanguage as MLanguage
import Models.MTextbook
import Text.Show.Unicode

someFunc :: IO ()
someFunc = do
    mapM_ uprint =<< MLanguage.getData
