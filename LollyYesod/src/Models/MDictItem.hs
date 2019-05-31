{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Models.MDictItem
    ( MDictItem(..)
    , fDICTID
    , fDICTNAME
    ) where

import Control.Lens
import Data.Default.Class
import Data.Text (Text)
import GHC.Generics (Generic)
import Helpers

data MDictItem = MDictItem
    { _fDICTID :: Text
    , _fDICTNAME :: Text
    } deriving (Show, Generic, Default)
makeLenses ''MDictItem