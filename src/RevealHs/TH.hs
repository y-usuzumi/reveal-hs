{-# LANGUAGE TemplateHaskell #-}

module RevealHs.TH where

import           Data.Hashable
import           Data.HashMap.Strict        as HM
import           Data.IORef
import           Data.Maybe
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           RevealHs.Internal          as I
import           RevealHs.QQ
import           System.IO.Unsafe

instance Hashable PkgName
instance Hashable ModName
instance Hashable Module

type SlidesMap = HashMap Module [Slide]

{-# NOINLINE slidesRef #-}
slidesRef :: IORef SlidesMap
slidesRef = unsafePerformIO $ newIORef empty

slide :: Slide -> DecsQ
slide s = do
  mod <- thisModule
  runIO $ do
    modifyIORef' slidesRef (alter addSlide mod)
    return []
  where
    addSlide Nothing       = Just [s]
    addSlide (Just slides) = Just (s:slides)

printEverything :: DecsQ
printEverything = [d|main = print $slides|]
  where
    slides :: ExpQ
    slides = runIO $ do
      slides <- readIORef slidesRef
      runQ $ liftData slides

mkRevealPage :: DecsQ
mkRevealPage = [d|main = putStrLn $export|]
  where
    export :: ExpQ
    export = do
      mod <- thisModule
      runIO $ do
        slides <- (fromJust . HM.lookup mod) <$> readIORef slidesRef
        return $ I.exportRevealPage slides
      >>= stringE
