{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveLift         #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}

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
           | MarkdownBlock String
           | TableBlock [[Block]]
           deriving (Data, Lift, Show)

data Slide = BlockSlide Block SlideOptions
           | MarkdownSlide String SlideOptions
           deriving (Data, Lift, Show)

data SlideOptions = SlideOptions { stretch :: Bool
                                 }
                  deriving (Data, Lift, Show)

defSlideOptions :: SlideOptions
defSlideOptions = SlideOptions { stretch = False
                               }

renderSlide :: Slide -> String
renderSlide s = case s of
  BlockSlide blk SlideOptions{..} ->
    [i|<section #{stretchToClass stretch}>#{renderBlock blk}</section>|]
  MarkdownSlide text SlideOptions{..} ->
    [i|<section #{stretchToClass stretch} data-markdown>#{renderMarkdown text}</section>|]
  where
    stretchToClass stretch =
      if stretch then [i|class="stretch"|] else ""

renderBlock :: Block -> String
renderBlock blk = case blk of
  TextBlock text ->
    [i|<p>#{text}</p>|]
  MarkdownBlock text ->
    [i|
  <p data-markdown>
    <script type="text/template">
#{text}
    </script>
  </p>
|]
  TableBlock tbl ->
    renderTable tbl
  where
    renderTable rows = [i|#{renderRows rows}|]
      where
        renderRows = intercalate [i|\n<div style="clear: both"></div>\n|] . map renderRow
    renderRow cells =
      [i|#{renderCells cells}|]
      where
        renderCells = intercalate "\n" . map (renderCell width)
        width = 100.0 / fromIntegral (length cells) - 1
    renderCell width blk' =
      [i|<div style="float: left; width: #{width}%">#{renderBlock blk'}</div>|]

renderMarkdown :: String -> String
renderMarkdown text =
  [i|
  <script type="text/template">
#{text}
  </script>
|]

exportRevealPage :: RevealOptions -> SlideMap -> [Module] -> String
exportRevealPage ro@RevealOptions{..} slides slideGroupOrder = [i|
<!DOCTYPE html>
<html>
    <head>
        <link rel="stylesheet" href="#{revealJsRoot}/css/reveal.css">
        <link rel="stylesheet" href="#{revealJsRoot}/css/theme/#{theme}.css">
        <link rel="stylesheet" href="#{revealJsRoot}/lib/css/#{codeTheme}.css">
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
