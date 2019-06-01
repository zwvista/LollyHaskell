{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Models.MVoice
    ( MVoice(..)
    , fID
    , fLANGID
    , fVOICETYPEID
    , fVOICELANG
    , fVOICENAME
    ) where

import Control.Lens
import Data.Default
import Data.Text (Text)
import GHC.Generics (Generic)

data MVoice = MVoice
    { _fID :: Int
    , _fLANGID :: Int
    , _fVOICETYPEID :: Int
    , _fVOICELANG :: Maybe Text
    , _fVOICENAME :: Text
    } deriving (Show, Generic, Default)
makeLenses ''MVoice
