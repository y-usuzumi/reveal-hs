module RevealHs.QQ where

import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax
import           RevealHs.Internal

slideQQ :: QuasiQuoter
slideQQ = QuasiQuoter { quoteExp = parseExp }

parseExp :: String -> ExpQ
parseExp a = liftData $ Slide $ TextBlock a
