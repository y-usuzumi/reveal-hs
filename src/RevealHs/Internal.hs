{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveLift         #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE RecordWildCards    #-}

module RevealHs.Internal where

import           Data.Data
import           Data.Hashable
import qualified Data.HashMap.Strict        as HM
import           Data.List
import           Data.String.Interpolate
import           Language.Haskell.TH.Syntax
import           RevealHs.Options

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

data Slide = Slide Block
              | MarkdownSlide String
              deriving (Data, Lift, Show)

renderSlide :: Slide -> String
renderSlide (Slide (TextBlock text)) = [i|<section>#{text}</section|]
renderSlide (MarkdownSlide text) = [i|<section data-markdown>
  <script type="text/template">
#{text}
  </script>
</section>
|]

exportRevealPage :: RevealOptions -> SlideMap -> [Module] -> String
exportRevealPage ro@RevealOptions{..} slides slideGroupOrder = [i|
<!DOCTYPE html>
<html>
    <head>
        <link rel="stylesheet" href="#{revealJsRoot}/css/reveal.css">
        <link rel="stylesheet" href="#{revealJsRoot}/css/theme/#{theme}.css">
        <script src="https://cdnjs.cloudflare.com/ajax/libs/headjs/1.0.3/head.js"></script>
    </head>
    <body>
        <div class="reveal">
            <div class="slides">
#{renderGroup $ map (slides HM.!) slideGroupOrder}
            </div>
        </div>
        <script src="#{revealJsRoot}/js/reveal.js"></script>
        <script>
            Reveal.initialize(#{revealOptionsToInitializeParams ro});
        </script>
    </body>
</html>|]
  where
    renderGroup :: [[Slide]] -> String
    renderGroup = intercalate "\n" . map (\s -> [i|<section>#{renderSlides s}</section>|])
    renderSlides :: [Slide] -> String
    renderSlides = intercalate "\n" . map renderSlide . reverse
