{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Models.MDictGroup
    ( MDictGroup(..)
    , fDICTID
    , fDICTNAME
    ) where

import Control.Lens
import Data.Text (Text)
import GHC.Generics

data MDictGroup = MDictGroup
    { _fDICTID :: Text
    , _fDICTNAME :: Text
    } deriving (Show, Generic)
makeLenses ''MDictGroup
