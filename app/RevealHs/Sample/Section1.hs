{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module RevealHs.Sample.Section1 where

import           RevealHs.Internal
import           RevealHs.QQ
import           RevealHs.TH

slide [md|
## Section 1

Slide 1

|]

slide [md|
## Section 1

Slide 2

|]

slide $ BlockSlide $ TableBlock
  [ [ [mdb|
### GG
      |]
    , TextBlock "HH"
    ]
  , [ TextBlock "JJ"
    , TextBlock "KK"
    ]
  ]
