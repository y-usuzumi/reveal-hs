{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveLift         #-}
{-# LANGUAGE QuasiQuotes        #-}

module RevealHs.Internal where

import           Data.Data
import           Data.List
import           Data.String.Interpolate
import           Language.Haskell.TH.Syntax

data Block = TextBlock String
           | TableBlock Table
           deriving (Data, Lift, Show)


--------
-- Table
--------

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

exportRevealPage :: [Slide] -> String
exportRevealPage slides = [i|
<!DOCTYPE html>
<html>
    <head>
        <link rel="stylesheet" href="css/reveal.css">
        <link rel="stylesheet" href="css/theme/white.css">
    </head>
    <body>
        <div class="reveal">
            <div class="slides">
#{render slides}
            </div>
        </div>
        <script src="js/reveal.js"></script>
        <script>
            Reveal.initialize();
        </script>
    </body>
</html>|]
  where
    render :: [Slide] -> String
    render = intercalate "\n" . map (\r -> [i|<section>#{renderSlide r}</section>|])
