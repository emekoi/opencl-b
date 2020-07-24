{-# LANGUAGE TemplateHaskell #-}

module Foreign.C.Convertable.TH
  ( deriveConvertable,
    deriveConvertableB,
    convertableType,
    convertableTypeB,
  )
where

import Control.Monad
import Data.Bifunctor (first, second)
import Data.Bits
import Foreign.C.Convertable
import Language.Haskell.TH

deriveConvertable :: [(Name, String)] -> Name -> Name -> DecsQ
deriveConvertable p c haskell = do
  fmap (: []) . instanceD (cxt []) (appT (appT (conT ''Convertable) (conT c)) (conT haskell)) $
    [ funD 'fromC [genFromC p'],
      funD 'toC (map genToC p')
    ]
  where
    p' = map (second mkName) p
    genFromC p = do
      let vP = varP $ mkName "v"
          gE (hT, cT) = normalGE [|v == $(varE cT)|] (conE hT)
          hTName = nameBase haskell
          cTName = nameBase c
          failG =
            normalGE
              (varE 'otherwise)
              [|error $ "Covertable " ++ cTName ++ " " ++ hTName ++ " fromC: " ++ show v|]
      clause [vP] (guardedB $ foldr (\v acc -> gE v : acc) [failG] p) []
    genToC (hT, cT) = do
      clause [conP hT []] (normalB (varE cT)) []

deriveConvertableB :: [(Name, String)] -> Name -> Name -> DecsQ
deriveConvertableB p c haskell = do
  [cI] <- deriveConvertable p c haskell
  TyConI (DataD _ _ _ _ c _) <- reify haskell
  cB <-
    [d|
      instance Convertable CInt [$(conT haskell)] where
        toC = foldl (.|.) 0 . map toC
        fromC v =
          filter
            (\c -> toC c .&. v == toC c)
            $(ListE <$> mapM conL c)
      |]
  return (cI : cB)
  where
    conL c
      | NormalC n _ <- c = conE n
      | otherwise = error "invalid Constructor type"

defineType :: [Name] -> Name -> DecsQ
defineType c haskell = do
  (: []) <$> dataD (cxt []) haskell [] Nothing (map (`normalC` []) c) [derivClause Nothing derived]
  where
    derived = map conT [''Show, ''Eq]

convertableType :: [(String, String)] -> Name -> String -> DecsQ
convertableType p c haskell = do
  liftM2
    (++)
    (defineType (map (mkName . fst) p) haskell')
    (deriveConvertable p' c haskell')
  where
    haskell' = mkName haskell
    p' = map (first mkName) p

convertableTypeB :: [(String, String)] -> Name -> String -> DecsQ
convertableTypeB p c haskell = do
  [hT] <- defineType conName haskell'
  [cI] <- deriveConvertable p' c haskell'
  [cB] <-
    [d|
      instance Convertable CInt [$(conT haskell')] where
        toC = foldl (.|.) 0 . map toC
        fromC v = filter (\c -> toC c .&. v == toC c) $(ListE <$> mapM conE conName)
      |]
  return [hT, cI, cB]
  where
    conName = map (mkName . fst) p
    haskell' = mkName haskell
    p' = map (first mkName) p
