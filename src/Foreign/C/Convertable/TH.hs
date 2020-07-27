{-# LANGUAGE TemplateHaskell #-}

module Foreign.C.Convertable.TH
  ( deriveConvertable,
    deriveConvertableB,
    convertableType,
    convertableTypeB,
    convertableTypeO,
    convertableTypeP,
  )
where

import Control.Monad
import Data.Bifunctor (bimap, first, second)
import Data.Bits
import Data.Char (toLower)
import Data.Typeable (Typeable)
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
      instance Convertable CUInt [$(conT haskell)] where
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
    derived = map conT [''Show, ''Eq, ''Typeable]

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
      instance Convertable CUInt [$(conT haskell')] where
        toC = foldl (.|.) 0 . map toC
        fromC v = filter (\c -> toC c .&. v == toC c) $(ListE <$> mapM conE conName)
      |]
  return [hT, cI, cB]
  where
    conName = map (mkName . fst) p
    haskell' = mkName haskell
    p' = map (first mkName) p

convertableTypeO :: String -> String -> DecsQ
convertableTypeO haskell c = do
  nT <- newtypeD (cxt []) haskell' [] Nothing (normalC haskell' [wrapped]) [derivClause Nothing derived]
  cB <-
    [d|
      instance Convertable $(cT) $(conT haskell') where
        toC $(conP haskell' [varP $ mkName "v"]) = v
        fromC = $(conE haskell')

      instance Show $(conT haskell') where
        show _ = $(litE . StringL $ "<" ++ haskell ++ ">")
      |]
  return $ nT : cB
  where
    (haskell', c') = (mkName haskell, mkName c)
    cT = appT (conT ''Ptr) (conT c')
    wrapped = (Bang NoSourceUnpackedness NoSourceStrictness,) <$> cT
    derived = map conT [''Eq, ''Typeable]

genPE :: String -> (PatQ, ExpQ)
genPE v = bimap (varP . mkName) (varE . mkName) (v, v)

deriveConvertableP :: [(String, TypeQ, String)] -> String -> String -> DecsQ
deriveConvertableP p c haskell = do
  (: [])
    <$> instanceD
      (cxt [])
      (appT (appT (conT ''Convertable) (conT c')) (conT haskell'))
      [ funD 'fromC [fromC'Clause],
        funD 'toC [toC'Clause]
      ]
  where
    (haskell', c') = (mkName haskell, mkName c)
    cFPrefix = map toLower c ++ "'"
    hConPE = map (\(x, _, _) -> genPE x) p
    cConPE = map (\(_, _, x) -> genPE $ cFPrefix ++ x) p
    toC'Clause =
      let patterns = [conP haskell' (map fst hConPE)]
          body = normalB . foldl appE (conE c') $ map (appE (varE 'toC) . snd) hConPE
       in clause patterns body []
    fromC'Clause =
      let patterns = [conP c' (map fst cConPE)]
          body = normalB . foldl appE (conE haskell') $ map (appE (varE 'fromC) . snd) cConPE
       in clause patterns body []

convertableTypeP :: [(String, TypeQ, String)] -> String -> String -> DecsQ
convertableTypeP p c haskell = do
  nT <- dataD (cxt []) haskell' [] Nothing [recC haskell' fields] [derivClause Nothing derived]
  cI <- deriveConvertableP p c haskell
  return $ nT : cI
  where
    defBang = Bang NoSourceUnpackedness NoSourceStrictness
    genField (fN, hT, _) = (mkName fN,defBang,) <$> hT
    fields = map genField p
    haskell' = mkName haskell
    derived = map conT [''Eq, ''Show, ''Typeable]
