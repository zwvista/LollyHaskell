module Lib
    ( someFunc
    ) where

import MDictionary
import MLangPhrase
import MLanguage
import MTextbook

someFunc :: IO ()
someFunc = do
    print =<< getDictsOnlineByLang 3
    print =<< getLanguages
    print =<< getTextbooks 3
