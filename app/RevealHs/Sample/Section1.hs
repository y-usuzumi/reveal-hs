{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module RevealHs.Sample.Section1 where

import           RevealHs

outerOptions defOuterOptions{ outerWidth = Pixels 1024
                            }

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
