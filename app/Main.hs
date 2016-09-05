{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Data.IORef
import           RevealHs.Internal
import           RevealHs.QQ
import           RevealHs.TH
import           System.IO.Unsafe

slide [md|
# Hello

## You idiot
|]

slide [md|
# A monad is just a monoid in the category of endofunctors.
|]

mkRevealPage
