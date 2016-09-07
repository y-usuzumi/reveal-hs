{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           RevealHs
import           RevealHs.Sample.Cover
import           RevealHs.Sample.Section1
import           RevealHs.Sample.Section2

mkRevealPage def { revealJsRoot = "/home/kj/Lab/external/reveal.js"
                 , theme = "moon"
                 , transition = TransitionSlide
                 , width = Pixels 1024
                 }
