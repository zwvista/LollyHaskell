module Lib
    ( someFunc
    ) where

import Models.MDictionary
import Models.MLangPhrase
import Models.MLanguage
import Models.MTextbook
import Text.Show.Unicode

someFunc :: IO ()
someFunc = do
    -- mapM_ uprint =<< getDictsOnlineByLang 3
    mapM_ uprint =<< getLanguages
    -- mapM_ uprint =<< getTextbooksByLang 3
