{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Data.IORef
import           RevealHs.Internal
import           RevealHs.QQ
import           RevealHs.TH
import           System.IO.Unsafe

slide [slideQQ| GG |]
slide [slideQQ| HH |]
slide [slideQQ| II |]

mkRevealPage
