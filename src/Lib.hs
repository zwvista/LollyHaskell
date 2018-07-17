module Lib
    ( someFunc
    ) where

import MDictionary

someFunc :: IO ()
someFunc = print =<< getDictsOnlineByLang 3
