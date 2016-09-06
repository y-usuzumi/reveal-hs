{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           RevealHs.Internal
import           RevealHs.Options
import           RevealHs.Sample.Cover
import           RevealHs.Sample.Section1
import           RevealHs.Sample.Section2
import           RevealHs.TH

mkRevealPage def { revealJsRoot = "/home/kj/Lab/external/reveal.js"
                 , theme = "moon"
                 , transition = TransitionSlide
                 }
