{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Computes free term variables.
module Free (Free (..)) where

import Data.List.NonEmpty qualified as NE
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Syntax

class Free v a | a -> v where
  freeVars :: (Ord v) => a -> Set v

instance (Free v a) => Free v [a] where
  freeVars = foldMap freeVars

instance (Free v a) => Free v (NE.NonEmpty a) where
  freeVars = foldMap freeVars

instance Free v (PatBase te f v) where
  freeVars PatId {} = S.empty

instance Free v (AtomBase te tp f v) where
  freeVars Base {} = S.empty
  freeVars (Lambda pat body _ _) =
    S.delete (patVar pat) $ freeVars body
  freeVars (TLambda _ body _ _) = freeVars body
  freeVars (ILambda _ body _ _) = freeVars body
  freeVars (Box _ body _ _ _) = freeVars body

instance Free v (BindBase te tp f v) where
  freeVars (BindVal _ _ e _) = freeVars e
  freeVars BindType {} = S.empty
  freeVars BindISpace {} = S.empty
  freeVars (BindFun _ params _ body _ _) =
    freeVars body S.\\ foldMap (S.singleton . patVar) params
  freeVars (BindTFun _ _ _ body _ _) = freeVars body
  freeVars (BindIFun _ _ _ body _ _) = freeVars body

instance Free v (ExpBase te tp f v) where
  freeVars (Var x _ _) = S.singleton x
  freeVars (Array _ as _ _) = freeVars as
  freeVars EmptyArray {} = S.empty
  freeVars (Frame _ es _ _) = freeVars es
  freeVars EmptyFrame {} = S.empty
  freeVars (App f arg _ _) = freeVars f `S.union` freeVars arg
  freeVars (TApp e _ _ _) = freeVars e
  freeVars (IApp e _ _ _) = freeVars e
  freeVars (Unbox _ x box body _ _) =
    freeVars box `S.union` S.delete x (freeVars body)
  freeVars (Let bs body _ _) =
    foldr step (freeVars body) bs
    where
      step b acc = freeVars b `S.union` maybe id S.delete (bindName b) acc

instance Free v (DeclBase te tp f v) where
  freeVars (Def b) = freeVars b
  freeVars (Entry _ params _ body _ _) =
    freeVars body S.\\ foldMap (S.singleton . patVar) params

instance Free v (ProgBase te tp f v) where
  freeVars (Prog decs) =
    freeVars decs S.\\ S.fromList (mapMaybe declName decs)
