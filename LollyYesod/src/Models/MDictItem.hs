{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Models.MDictItem
    ( MDictItem(..)
    , fDICTID
    , fDICTNAME
    ) where

import Control.Lens
import Data.Text (Text)
import GHC.Generics

data MDictItem = MDictItem
    { _fDICTID :: Text
    , _fDICTNAME :: Text
    } deriving (Show, Generic)
makeLenses ''MDictItem
