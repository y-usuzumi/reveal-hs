module RevealHs.Sample.Cover where

import           RevealHs
import           RevealHs.Sample.Env
import           System.IO.Unsafe

slide [md|
# Introduction to Reveal-Hs

by [薄墨ゆきお](https://github.com/KenetJervet)
(written on #{unsafePerformIO today})

History:

|Version|Change logs|
|---|---|
|0.0.0.1|Added markdown support|
|0.0.0.2|Markdown QQ supports string interpolation|

|]
