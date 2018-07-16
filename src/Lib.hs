module Lib
    ( someFunc
    ) where

import MDictionary

someFunc :: IO ()
someFunc = print =<< getDataByLang 3
