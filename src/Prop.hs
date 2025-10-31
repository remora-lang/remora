{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Prop
  ( arrayTypeOf,
    scalarTypeOf,
    baseTypeOf,
    HasType (..),
    HasShape (..),
    HasSrcPos (..),
    IsType (..),
    (@=),
    (\\),
    (.<=.),
    maximumShape,
  )
where

import Control.Monad
import Data.Maybe
import Prettyprinter
import Shape
import Substitute
import Symbolic qualified
import Syntax
import Util
import VName

arrayTypeOf :: Exp Info VName -> ArrayType Info VName
arrayTypeOf = normType . arrayTypeOf_
  where
    arrayTypeOf_ (Var _ (Info t) _) = t
    arrayTypeOf_ (Array _ _ (Info t) _) = t
    arrayTypeOf_ (EmptyArray _ _ (Info t) _) = t
    arrayTypeOf_ (Frame _ _ (Info t) _) = t
    arrayTypeOf_ (EmptyFrame _ _ (Info t) _) = t
    arrayTypeOf_ (App _ _ (Info (t, _)) _) = t
    arrayTypeOf_ (TApp _ _ (Info t) _) = t
    arrayTypeOf_ (IApp _ _ (Info t) _) = t
    arrayTypeOf_ (Unbox _ _ _ _ (Info t) _) = t
    arrayTypeOf_ (Let _ _ (Info t) _) = t

scalarTypeOf :: Atom Info VName -> ScalarType Info VName
scalarTypeOf = normType . scalarTypeOf_
  where
    scalarTypeOf_ (Base _ (Info t) _) = t
    scalarTypeOf_ (Lambda _ _ (Info t) _) = t
    scalarTypeOf_ (TLambda _ _ (Info t) _) = t
    scalarTypeOf_ (ILambda _ _ (Info t) _) = t
    scalarTypeOf_ (Box _ _ t _) = t

baseTypeOf :: Base -> ScalarType f VName
baseTypeOf BoolVal {} = Bool
baseTypeOf IntVal {} = Int
baseTypeOf FloatVal {} = Float

-- | Things that have a type.
class HasType x where
  -- | Returns a normalized type.
  typeOf :: x -> Type Info VName
  typeOf = normType . typeOf_

  typeOf_ :: x -> Type Info VName

instance HasType Base where
  typeOf_ = ScalarType . baseTypeOf

instance HasType (Atom Info VName) where
  typeOf_ = ScalarType . scalarTypeOf

instance HasType (Exp Info VName) where
  typeOf_ = ArrayType . arrayTypeOf

-- | Things that have a shape.
class HasShape x where
  shapeOf :: x -> Shape VName
  shapeOf = normShape . shapeOf_

  shapeOf_ :: x -> Shape VName

instance HasShape (Atom f VName) where
  shapeOf_ _ = mempty

instance HasShape (ArrayType Info VName) where
  shapeOf_ (A _ s) = s
  shapeOf_ (ArrayTypeVar _ _ (Info s)) = ShapeVar s

instance HasShape (Type Info VName) where
  shapeOf_ (ArrayType t) = shapeOf t
  shapeOf_ _ = mempty

instance HasShape (Exp Info VName) where
  shapeOf_ = shapeOf_ . typeOf_

-- | Things that have a source position.
class HasSrcPos x where
  posOf :: x -> SourcePos

instance HasSrcPos (Bind f v) where
  posOf (BindVal _ _ _ pos) = pos
  posOf (BindFun _ _ _ _ pos) = pos
  posOf (BindTFun _ _ _ _ pos) = pos
  posOf (BindIFun _ _ _ _ pos) = pos
  posOf (BindType _ _ pos) = pos
  posOf (BindIdx _ _ pos) = pos

instance HasSrcPos (Atom f v) where
  posOf (Base _ _ pos) = pos
  posOf (Lambda _ _ _ pos) = pos
  posOf (TLambda _ _ _ pos) = pos
  posOf (ILambda _ _ _ pos) = pos
  posOf (Box _ _ _ pos) = pos

instance HasSrcPos (Exp f v) where
  posOf (Var _ _ pos) = pos
  posOf (Array _ _ _ pos) = pos
  posOf (EmptyArray _ _ _ pos) = pos
  posOf (Frame _ _ _ pos) = pos
  posOf (EmptyFrame _ _ _ pos) = pos
  posOf (App _ _ _ pos) = pos
  posOf (TApp _ _ _ pos) = pos
  posOf (IApp _ _ _ pos) = pos
  posOf (Unbox _ _ _ _ _ pos) = pos
  posOf (Let _ _ _ pos) = pos

-- | Things that are types.
class IsType x where
  normType :: x -> x
  (~=) :: (MonadVName m) => x -> x -> m Bool

infix 4 ~=

instance IsType (ScalarType Info VName) where
  normType (ts :-> r) = map normType ts :-> normType r
  normType (Forall pts t) = Forall pts $ normType t
  normType (Pi pts t) = Pi pts $ normType t
  normType (Sigma pts t) = Sigma pts $ normType t
  normType t = t

  (ps :-> r) ~= (qs :-> t)
    | length ps == length qs =
        andM (zipWith (~=) ps qs) ^&& (r ~= t)
  Forall ps r ~= Forall qs t
    | length ps == length qs = do
        xs <- forM ps $ \p -> do
          vname <- newVName $ prettyText p
          pure $ vname <$ p
        substitute' (zip ps xs) r ~= substitute' (zip qs xs) t
  Pi ps r ~= Pi qs t
    | length ps == length qs = do
        xs <- forM ps $ \p -> do
          vname <- newVName $ prettyText p
          pure $ vname <$ p
        substitute' (zip ps xs) r ~= substitute' (zip qs xs) t
  Sigma ps r ~= Sigma qs t
    | length ps == length qs = do
        xs <- forM ps $ \p -> do
          vname <- newVName $ prettyText p
          pure $ vname <$ p
        substitute' (zip ps xs) r ~= substitute' (zip qs xs) t
  t ~= r = pure $ t == r

instance IsType (ArrayType Info VName) where
  normType (A t s) = A (normType t) (normShape s)
  normType t = t

  A t s ~= A y x =
    (t ~= y) ^&& pure (s Symbolic.@= x)
  t ~= r = pure $ t == r

instance IsType (Type Info VName) where
  normType (ScalarType t) = ScalarType $ normType t
  normType (ArrayType t) = ArrayType $ normType t

  ScalarType t ~= ScalarType r = t ~= r
  ArrayType t ~= ArrayType r = t ~= r
  _ ~= _ = pure False

-- | Naive shape equality.
(@=) :: (Ord v) => Shape v -> Shape v -> Bool
s @= t = normShape s == normShape t

infix 4 @=

-- | Shape suffix subtraction; given shapes @(++ s1 s2)@ and @t@ if @t == s2@ then
-- returns @Just s1@. Otherwise fails with @Nothing@.
(\\) :: (Eq v, Ord v, Show v) => Shape v -> Shape v -> Maybe (Shape v)
s \\ t
  | s @= t = Just mempty
Concat [] \\ _ = Nothing
s \\ Concat [] = pure s
(Concat ss) \\ (Concat ts)
  | last ss @= last ts = Concat (init ss) \\ Concat (init ts)
(Concat ss) \\ t
  | last ss @= t = pure $ Concat $ init ss
s \\ t = error $ show (s, t)

-- | @s .<= t@ is true if @s@ is a suffix of @t@.
(.<=.) :: (Eq v, Ord v, Show v) => Shape v -> Shape v -> Bool
s .<=. t = isJust $ t \\ s

-- | Returns the largest shape from a collection of shapes with a common prefix.
-- Unsafe to use if the shapes do not have a common prefix.
maximumShape :: (Ord v, Show v, Pretty v, Foldable t) => t (Shape v) -> Shape v
maximumShape =
  foldr
    ( \next shape ->
        if shape .<=. next
          then next
          else shape
    )
    mempty
    . foldMap ((\x -> [x]) . normShape)
