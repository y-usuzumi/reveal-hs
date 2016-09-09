{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module RevealHs.QQ where

import           Data.String.Interpolate
import           Data.Text.Lazy             as T
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
parseTextSlide s = [|BlockSide (TextBlock s)|]

parseMarkdownSlide :: String -> ExpQ
parseMarkdownSlide s = [|MarkdownSlide $interpolatedS|]
  where
    interpolatedS = quoteExp i s

parseMarkdownBlock :: String -> ExpQ
parseMarkdownBlock a = [|MarkdownBlock a|]
