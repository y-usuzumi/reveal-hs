{-# LANGUAGE TemplateHaskell #-}

module RevealHs.QQ where

import           Data.Text.Lazy                as T
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax
import           RevealHs.Internal

slideQQ :: QuasiQuoter
slideQQ = QuasiQuoter { quoteExp = parseTextSlide }

md :: QuasiQuoter
md = QuasiQuoter { quoteExp = parseMarkdownSlide }

mdb :: QuasiQuoter
mdb = QuasiQuoter { quoteExp = parseMarkdownBlock }

parseTextSlide :: String -> ExpQ
parseTextSlide a = [|BlockSide (TextBlock a)|]

parseMarkdownSlide :: String -> ExpQ
parseMarkdownSlide a = [|MarkdownSlide a|]

parseMarkdownBlock :: String -> ExpQ
parseMarkdownBlock a = [|MarkdownBlock a|]
