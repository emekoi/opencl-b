{-# LANGUAGE TemplateHaskell #-}

module Foreign.C.Convertable.TH
  ( deriveConvertable,
    deriveConvertableBitfield,
  )
where

import Control.Monad
import Data.Bifunctor (bimap, second)
import Data.Bits
import Foreign.C.Convertable
import Language.Haskell.TH

deriveConvertable :: [(Name, String)] -> Name -> Name -> DecsQ
deriveConvertable p c haskell = do
  fmap (: []) . instanceD (cxt []) (appT (appT (conT ''Convertable) (conT c)) (conT haskell)) $
    [ funD (mkName "fromC") [genFromC p'],
      funD (mkName "toC") (map genToC p')
    ]
  where
    p' = map (second mkName) p
    genFromC p = do
      let (vP, vE) = genPE "v"
          gE (hT, cT) = normalGE [|v == $(varE cT)|] (conE hT)
          hTName = nameBase haskell
          cTName = nameBase c
          failG =
            normalGE
              (varE 'otherwise)
              [|error $ "Covertable " ++ cTName ++ " " ++ hTName ++ " fromC: " ++ show v|]
      clause [vP] (guardedB $ foldr (\v acc -> gE v : acc) [failG] p') []
    genToC (hT, cT) = do
      clause [conP hT []] (normalB (varE cT)) []

genPE :: String -> (PatQ, ExpQ)
genPE v = bimap (varP . mkName) (varE . mkName) (v, v)

deriveConvertableBitfield :: [(Name, String)] -> Name -> Name -> DecsQ
deriveConvertableBitfield p c haskell = do
  [cI] <- deriveConvertable p c haskell
  TyConI (DataD _ _ _ _ c _) <- reify haskell
  let conL = ListE <$> mapM (conE . (\(NormalC n _) -> n)) c
  [cB] <-
    [d|
      instance Convertable CInt [$(conT haskell)] where
        toC = foldl (.|.) 0 . map toC
        fromC v = filter (\c -> toC c .&. v == toC c) $(conL)
      |]
  return [cI, cB]
