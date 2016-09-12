{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module RevealHs.TH where

import           Control.Monad
import           Data.HashMap.Strict        as HM
import           Data.IORef
import           Data.Maybe
import           Data.String.Interpolate
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           RevealHs.Internal          as I
import           RevealHs.Options
import           RevealHs.QQ
import           System.IO.Unsafe

{-# NOINLINE slidesRef #-}
slidesRef :: IORef SlideMap
slidesRef = unsafePerformIO $ newIORef empty

{-# NOINLINE slideGroupOrderRef #-}
slideGroupOrderRef :: IORef [Module]
slideGroupOrderRef = unsafePerformIO $ newIORef []

slide :: (SlideOptions -> Slide) -> DecsQ
slide = slide' defSlideOptions

slide' :: SlideOptions -> (SlideOptions -> Slide) -> DecsQ
slide' so s = do
  mod <- thisModule
  runIO $ do
    slides <- readIORef slidesRef
    when (isNothing $ HM.lookup mod slides) $
      modifyIORef' slideGroupOrderRef (mod:)
    modifyIORef' slidesRef (alter addSlide mod)
    return []
  where
    addSlide Nothing = Just (defGroupOptions, [s so])
    addSlide (Just (opts, slides)) = Just (opts, s so:slides)

groupOptions :: GroupOptions -> DecsQ
groupOptions opts = do
  mod <- thisModule
  runIO $ do
    slides <- readIORef slidesRef
    when (isNothing $ HM.lookup mod slides) $
      modifyIORef' slideGroupOrderRef (mod:)
    modifyIORef' slidesRef (alter setOpts mod)
    return []
  where
    setOpts Nothing = Just (opts, [])
    setOpts (Just (_, slides)) = Just (opts, slides)

mkRevealPage :: RevealOptions -> DecsQ
mkRevealPage ro = [d|main = putStrLn $export|]
  where
    export :: ExpQ
    export = do
      s <- runIO $ do
        slides <- readIORef slidesRef
        slideGroupOrder <- readIORef slideGroupOrderRef
        return $ I.exportRevealPage ro slides slideGroupOrder
      stringE s
