{-# LANGUAGE TemplateHaskell #-}

module Data.Case.Enumerate 
  ( enumerateConstructors
  , enumerateConstructors'
  ) where

import Language.Haskell.TH
import Control.Monad

{-| This will create a pattern match on the first argument
    that splices in the third argument for each pattern.
    Example:

 > data Color = Red | Blue | Green deriving Show
 > myFunc :: Color -> String
 > myFunc c = $(enumerateConstructors 'c ''Color =<< [|show c|])

   I should fix this function to actually make it work that
   way. It actually uses the singletonized data type instead.
-}

enumerateConstructors :: Name -> Name -> Q Exp -> Q Exp
enumerateConstructors vname name expr = do
  TyConI (DataD _ _ _ ctors _) <- reify name
  caseE (varE vname) (map mkMatch ctors)
  where
    mkMatch (NormalC cname []) =
        match (conP (sketchyNameSingletonize cname) []) (normalB expr) []
    mkMatch _ =
        fail "constConstructors2: empty data constructor required"

enumerateConstructors' :: Name -> Q Exp -> Q Exp
enumerateConstructors' name expr = do
  TyConI (DataD _ _ _ ctors _) <- reify name
  let alts = map mkMatch ctors
  [| \x -> $(caseE [| x |] alts) |]
  where
    mkMatch (NormalC cname []) =
        match (conP (sketchyNameSingletonize cname) []) (normalB expr) []
    mkMatch _ =
        fail "constConstructors2: empty data constructor required"


sketchyNameSingletonize :: Name -> Name
sketchyNameSingletonize = id
  . mkName . ('S':) . reverse 
  . takeWhile (/= '.') . reverse . show

{-}
DataInstD 
  []
  -- ^ a Cxt or [Pred], the type class context 
  Sing 
  -- ^ The Name of the class/family
  [ SigT (VarT z) (ConT GHC.Types.Bool) ] 
  -- ^ A [Type]. This is (z :: Bool)
  [ ForallC 
    -- ^ has type Con, so is a GADT
      [] 
      -- ^ TyVarBndr
      [AppT (AppT EqualityT (VarT z)) (ConT GHC.Types.False)]
      -- ^ Cxt, list of predicates. in this case, we're asserting that 
      -- `z ~ False`.
      (NormalC Data.Singletons.Prelude.Instances.SFalse [])
      -- ^ Normal constructor.
  , ForallC 
      [] 
      [AppT (AppT EqualityT (VarT z0_1627472669)) (ConT GHC.Types.True)] 
      (NormalC Data.Singletons.Prelude.Instances.STrue [])
  ]
  -- ^ [Con], the constructors we want
  []
  -- ^ [Name] of instances to derive
-}

