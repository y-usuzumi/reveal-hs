module RevealHs.QQ where

import           Data.Text.Lazy                as T
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax
import           RevealHs.Internal
import           Text.Blaze.Html.Renderer.Text
import           Text.Markdown

slideQQ :: QuasiQuoter
slideQQ = QuasiQuoter { quoteExp = parseExp }

md :: QuasiQuoter
md = QuasiQuoter { quoteExp = parseMarkdown }

parseExp :: String -> ExpQ
parseExp a = liftData $ Slide $ TextBlock a

parseMarkdown :: String -> ExpQ
parseMarkdown = liftData
                . Slide . TextBlock
                . T.unpack
                . renderHtml
                . markdown def
                . T.pack
