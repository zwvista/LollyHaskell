{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Helpers
    ( customOptionsLolly
    , urlLolly
    , aesonDrop
    ) where

import Data.Aeson.Types
import Network.HTTP.Req
import Data.Default.Class
import Data.Text (Text)

-- Data.Aeson.Casing
aesonDrop :: Int -> (String -> String) -> Options
aesonDrop n f = defaultOptions
        { fieldLabelModifier = f . drop n }

customOptionsLolly :: Options
customOptionsLolly = aesonDrop 2 id

urlLolly :: Url 'Https
urlLolly = https "zwvista.tk" /: "lolly" /: "api.php"

instance Default Text where def = "" :: Text