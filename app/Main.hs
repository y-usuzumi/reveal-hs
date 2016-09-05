{-# LANGUAGE TemplateHaskell #-}

module Main where

import RevealHs.TH
import System.IO.Unsafe
import Data.IORef

slide "Hello"
slide "World"
slide "Template Haskell Rocks!"

printEverything
