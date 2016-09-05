{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveLift         #-}
{-# LANGUAGE QuasiQuotes        #-}

module RevealHs.Internal where

import           Data.Data
import           Data.Hashable
import qualified Data.HashMap.Strict        as HM
import           Data.List
import           Data.String.Interpolate
import           Language.Haskell.TH.Syntax

type SlideMap = HM.HashMap Module [Slide]

instance Hashable PkgName
instance Hashable ModName
instance Hashable Module

--------
-- Slide hierarchy
--------

data Block = TextBlock String
           | TableBlock Table
           deriving (Data, Lift, Show)

newtype Cell = Cell [Block]
             deriving (Data, Lift, Show)

newtype Column = Column [Cell]
               deriving (Data, Lift, Show)

newtype Row = Row [Column]
            deriving (Data, Lift, Show)

newtype Table = Table [Row]
              deriving (Data, Lift, Show)

newtype Slide = Slide Block
              deriving (Data, Lift, Show)

renderSlide :: Slide -> String
renderSlide (Slide (TextBlock text)) = text

exportRevealPage :: SlideMap -> String
exportRevealPage slides = [i|
<!DOCTYPE html>
<html>
    <head>
        <link rel="stylesheet" href="file:///home/kj/Lab/external/reveal.js/css/reveal.css">
        <link rel="stylesheet" href="file:///home/kj/Lab/external/reveal.js/css/theme/moon.css">
    </head>
    <body>
        <div class="reveal">
            <div class="slides">
#{renderGroup $ HM.toList slides}
            </div>
        </div>
        <script src="file:///home/kj/Lab/external/reveal.js/js/reveal.js"></script>
        <script>
            Reveal.initialize();
        </script>
    </body>
</html>|]
  where
    renderGroup :: [(Module, [Slide])] -> String
    renderGroup = intercalate "\n" . map (\(m, s) -> [i|<section>#{renderSlides s}</section>|])
    renderSlides :: [Slide] -> String
    renderSlides = intercalate "\n" . map (\r -> [i|<section>#{renderSlide r}</section>|]) . reverse
