{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveLift            #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}

module RevealHs.Options where

import           Data.Aeson
import           Data.Data
import           Data.String.Interpolate
import           Data.Text                  as T
import           Language.Haskell.TH.Syntax

data CSSSize' a = Pixels a | Percentage a | NotSet
             deriving (Eq, Lift, Show, Functor)

type CSSSize = CSSSize' Int

cssSizeToRevealValue :: CSSSize -> String
cssSizeToRevealValue size = case size of
  Pixels px      -> [i|#{show px}|]
  Percentage pct -> [i|"#{show pct}%"|]
  NotSet         -> [i|"undefined"|]

cssSizeToCSSValue :: CSSSize -> String
cssSizeToCSSValue size = case size of
  Pixels px      -> [i|#{show px}px|]
  Percentage pct -> [i|"#{show pct}%"|]
  NotSet         -> error "Check it explicitly in your code and set your desired value"


data AutoSlideMethod = AutoSlideNavigateNext
                     deriving (Eq, Lift, Show)

data Transition = TransitionDefault
                | TransitionNone
                | TransitionFade
                | TransitionSlide
                | TransitionConvex
                | TransitionConcave
                | TransitionZoom
                deriving (Eq, Lift, Show)

data TransitionSpeed = TransitionSpeedDefault
                     | TransitionSpeedFast
                     | TransitionSpeedSlow
                     deriving (Eq, Lift, Show)

data RevealOptions =
  RevealOptions { revealJsRoot                  :: String
                , theme                         :: String
                , codeTheme                     :: String
                , customCSS                     :: String
                , controls                      :: Bool
                , progress                      :: Bool
                , slideNumber                   :: Bool
                , history                       :: Bool
                , keyboard                      :: Bool
                , overview                      :: Bool
                , center                        :: Bool
                , touch                         :: Bool
                , loop                          :: Bool
                , rtl                           :: Bool
                , shuffle                       :: Bool
                , fragments                     :: Bool
                , embedded                      :: Bool
                , help                          :: Bool
                , showNotes                     :: Bool
                , autoSlide                     :: Int
                , autoSlideStoppable            :: Bool
                , autoSlideMethod               :: AutoSlideMethod
                , mouseWheel                    :: Bool
                , hideAddressBarOnMobileDevices :: Bool
                , previewLinks                  :: Bool
                , transition                    :: Transition
                , transitionSpeed               :: TransitionSpeed
                , backgroundTransition          :: Transition
                , viewDistance                  :: Int
                , parallaxBackgroundImage       :: String
                , parallaxBackgroundSize        :: String
                , parallaxBackgroundHorizontal  :: Maybe Int
                , parallaxBackgroundVertical    :: Maybe Int
                , width                         :: CSSSize
                , height                        :: CSSSize
                , margin                        :: Double
                , minScale                      :: Double
                , maxScale                      :: Double
                }

def :: RevealOptions
def = RevealOptions { revealJsRoot = ""
                    , theme = "night"
                    , codeTheme = "zenburn"
                    , customCSS = ""
                    , controls = True
                    , progress = True
                    , slideNumber = True
                    , history = True
                    , keyboard = True
                    , overview = True
                    , center = True
                    , touch = True
                    , loop = False
                    , rtl = False
                    , shuffle = False
                    , fragments = True
                    , embedded = False
                    , help = True
                    , showNotes = False
                    , autoSlide = 0
                    , autoSlideStoppable = True
                    , autoSlideMethod = AutoSlideNavigateNext
                    , mouseWheel = False
                    , hideAddressBarOnMobileDevices = True
                    , previewLinks = False
                    , transition = TransitionDefault
                    , transitionSpeed = TransitionSpeedDefault
                    , backgroundTransition = TransitionDefault
                    , viewDistance = 3
                    , parallaxBackgroundImage = ""
                    , parallaxBackgroundSize = ""
                    , parallaxBackgroundHorizontal = Nothing
                    , parallaxBackgroundVertical = Nothing
                    , width = Pixels 960
                    , height = Pixels 700
                    , margin = 0.1
                    , minScale = 0.2
                    , maxScale = 1.5
                    }

revealOptionsToInitializeParams :: RevealOptions -> String
revealOptionsToInitializeParams RevealOptions{..} =
  [i|{
    controls: #{encode controls},
    progress: #{encode progress},
    slideNumber: #{encode slideNumber},
    history: #{encode history},
    keyboard: #{encode keyboard},
    overview: #{encode overview},
    center: #{encode center},
    touch: #{encode touch},
    loop: #{encode loop},
    rtl: #{encode rtl},
    shuffle: #{encode shuffle},
    fragments: #{encode fragments},
    embedded: #{encode embedded},
    help: #{encode help},
    showNotes: #{encode showNotes},
    autoSlide: #{encode autoSlide},
    autoSlideStoppable: #{encode autoSlideStoppable},
    autoSlideMethod: #{convertAutoSlideMethod autoSlideMethod},
    mouseWheel: #{encode mouseWheel},
    hideAddressBar: #{encode hideAddressBarOnMobileDevices},
    previewLinks: #{encode previewLinks},
    transition: #{convertTransition transition},
    transitionSpeed: #{convertTransitionSpeed transitionSpeed},
    backgroundTransition: #{convertTransition backgroundTransition},
    viewDistance: #{encode viewDistance},
    parallaxBackgroundImage: #{encode parallaxBackgroundImage},
    parallaxBackgroundSize: #{encode parallaxBackgroundSize},
    parallaxBackgroundHorizontal: #{encode parallaxBackgroundHorizontal},
    parallaxBackgroundVertical: #{encode parallaxBackgroundVertical},
    width: #{cssSizeToRevealValue width},
    height: #{cssSizeToRevealValue height},
    margin: #{encode margin},
    minScale: #{encode minScale},
    maxScale: #{encode maxScale},
    dependencies: [
        // Cross-browser shim that fully implements classList - https://github.com/eligrey/classList.js/
        { src: 'lib/js/classList.js', condition: function() { return !document.body.classList; } },

        // Interpret Markdown in <section> elements
        { src: '#{revealJsRoot}/plugin/markdown/marked.js', condition: function() { return !!document.querySelector( '[data-markdown]' ); } },
        { src: '#{revealJsRoot}/plugin/markdown/markdown.js', condition: function() { return !!document.querySelector( '[data-markdown]' ); } },

        // Syntax highlight for <code> elements
        { src: '#{revealJsRoot}/plugin/highlight/highlight.js', async: true, callback: function() { hljs.initHighlightingOnLoad(); } },

        // Zoom in and out with Alt+click
        { src: '#{revealJsRoot}/plugin/zoom-js/zoom.js', async: true },

        // Speaker notes
        { src: '#{revealJsRoot}/plugin/notes/notes.js', async: true },

        // MathJax
        { src: '#{revealJsRoot}/plugin/math/math.js', async: true }
    ]
    }|]
  where
    convertAutoSlideMethod AutoSlideNavigateNext = "Reveal.navigateNext"
    convertTransition trans = encode $ case trans of
      TransitionDefault -> "default"
      TransitionNone    -> "none"
      TransitionFade    -> "fade"
      TransitionSlide   -> "slide"
      TransitionConvex  -> "convex"
      TransitionConcave -> "concave"
      TransitionZoom    -> "zoom"
    convertTransitionSpeed speed = encode $ case speed of
      TransitionSpeedDefault -> "default"
      TransitionSpeedFast    -> "fast"
      TransitionSpeedSlow    -> "slow"


data GroupOptions = GroupOptions { groupWidth :: CSSSize
                                 , groupCSS   :: String
                                 }
                  deriving (Lift, Show)

defGroupOptions :: GroupOptions
defGroupOptions = GroupOptions { groupWidth = Pixels 960
                               , groupCSS = ""
                               }

data SlideOptions = SlideOptions { slidePadding :: CSSSize
                                 , slideCSS     :: String
                                 }
                  deriving (Lift, Show)

defSlideOptions :: SlideOptions
defSlideOptions = SlideOptions { slidePadding = NotSet
                               , slideCSS = ""
                               }
