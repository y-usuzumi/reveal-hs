{-# LANGUAGE TemplateHaskell #-}
module RevealHs.TH where

import Language.Haskell.TH
import System.IO.Unsafe
import Data.IORef

{-# NOINLINE slideCount #-}
slideCount :: IORef Int
slideCount = unsafePerformIO $ newIORef 0

mkSlideName :: Int -> Name
mkSlideName idx = mkName $ "__slide_" ++ show idx

nextSlideName :: Q Name
nextSlideName = runIO $ do
  slideSeq <- readIORef slideCount
  modifyIORef' slideCount (+1)
  return $ mkSlideName slideSeq

initialize :: DecsQ
initialize = return [ PragmaD $ InlineP (mkName "__slides") NoInline FunLike AllPhases
                    , SigD (mkName "__slides") (AppT (ConT (mkName "IORef")) (AppT ListT (ConT (mkName "String"))))
                    , ValD (VarP (mkName "__slides"))
                      (NormalB (AppE (VarE (mkName "unsafePerformIO")) (AppE (VarE (mkName "newIORef")) (ConE (mkName "[]"))))) []
                    ]

slide :: String -> DecsQ
slide a = do
  name <- nextSlideName
  [d|$(return $ VarP name) = $(return $ LitE (StringL a))|]

printEverything :: DecsQ
printEverything = [d|main = print $slides|]
  where
    slides :: Q Exp
    slides = runIO $ do
      currIdx <- readIORef slideCount
      return $ ListE $ map (VarE . mkSlideName) [0..currIdx-1]
