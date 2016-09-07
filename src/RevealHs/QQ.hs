module RevealHs.QQ where

import           Data.Text.Lazy                as T
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax
import           RevealHs.Internal

slideQQ :: QuasiQuoter
slideQQ = QuasiQuoter { quoteExp = parseExp }

md :: QuasiQuoter
md = QuasiQuoter { quoteExp = parseMarkdownSlide }

mdb :: QuasiQuoter
mdb = QuasiQuoter { quoteExp = parseMarkdownBlock }

tbl :: QuasiQuoter
tbl = QuasiQuoter { quoteExp = parseTable }

parseExp :: String -> ExpQ
parseExp a = liftData $ BlockSlide $ TextBlock a

parseMarkdownSlide :: String -> ExpQ
parseMarkdownSlide = liftData . MarkdownSlide

parseMarkdownBlock :: String -> ExpQ
parseMarkdownBlock = liftData . MarkdownBlock

parseTable :: String -> ExpQ
parseTable = undefined
