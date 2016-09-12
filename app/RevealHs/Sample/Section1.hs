module RevealHs.Sample.Section1 where

import           Data.String.Interpolate
import           RevealHs

groupOptions defGroupOptions{ groupWidth = Pixels 1024
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
