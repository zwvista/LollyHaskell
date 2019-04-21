{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Models.MAutoCorrect
    ( MAutoCorrect
    , fID
    , fLANGID
    , fSEQNUM
    , fINPUT
    , fEXTENDED
    , fBASIC
    , getDataByLang
    , autoCorrect
    ) where

import Control.Lens
import Data.Aeson
import Data.Default.Class
import Data.Text (Text, replace)
import GHC.Generics
import Helpers
import Network.HTTP.Req

data MAutoCorrect = MAutoCorrect
    { _fID :: Int
    , _fLANGID :: Int
    , _fSEQNUM :: Int
    , _fINPUT :: Text
    , _fEXTENDED :: Text
    , _fBASIC :: Text
    } deriving (Show, Generic)
makeLenses ''MAutoCorrect

instance ToJSON MAutoCorrect where
    toJSON = genericToJSON customOptionsLolly

instance FromJSON MAutoCorrect where
    parseJSON = genericParseJSON customOptionsLolly

newtype MAutoCorrects = MAutoCorrects{_fAUTOCORRECT :: [MAutoCorrect]} deriving (Show, Generic)

instance FromJSON MAutoCorrects where
    parseJSON = genericParseJSON customOptionsLolly

getDataByLang :: Int -> IO [MAutoCorrect]
getDataByLang langid = runReq def $ do
    v <- req GET (urlLolly /: "AUTOCORRECT") NoReqBody jsonResponse $
        "transform" =: (1 :: Int) <>
        "filter" =: ("LANGIDFROM,eq," ++ show langid)
    return $ _fAUTOCORRECT (responseBody v)

autoCorrect :: Text -> [MAutoCorrect] -> (MAutoCorrect -> Text) -> (MAutoCorrect -> Text) -> Text
autoCorrect text arrAutoCorrect colFunc1 colFunc2 =
    foldl (\str row -> replace str (colFunc1 row) (colFunc2 row)) text arrAutoCorrect
