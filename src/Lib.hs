module Lib
    ( someFunc
    ) where

import MDictionary
import MLangPhrase
import MLanguage
import MTextbook
import Text.Show.Unicode

someFunc :: IO ()
someFunc = do
    -- mapM_ uprint =<< getDictsOnlineByLang 3
    mapM_ uprint =<< getLanguages
    -- mapM_ uprint =<< getTextbooksByLang 3
