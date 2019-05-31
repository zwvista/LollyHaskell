{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Models.MSelectItem
    ( MSelectItem(..)
    , value
    , label
    ) where

import Control.Lens
import Data.Default.Class
import Data.Text (Text)
import GHC.Generics (Generic)
import Helpers

data MSelectItem = MSelectItem
    { _value :: Int
    , _label :: Text
    } deriving (Show, Generic, Default)
makeLenses ''MSelectItem
