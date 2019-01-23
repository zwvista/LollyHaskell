{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Models.MDictPicker
    ( MDictPicker(..)
    , fDICTID
    , fDICTNAME
    ) where

import Control.Lens
import Data.Text (Text)
import GHC.Generics

data MDictPicker = MDictPicker
    { _fDICTID :: Text
    , _fDICTNAME :: Text
    } deriving (Show, Generic)
makeLenses ''MDictPicker
