{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Syntax
  ( module Shape,
    SourcePos,
    Unchecked (..),
    Typed (..),
    TVar (..),
    unTVar,
    Type (..),
    elemType,
    atomKind,
    arrayKind,
    Base (..),
    Atom (..),
    Exp (..),
    arrayElems,
    frameElems,
    flattenExp,
    HasType (..),
    normType,
    HasShape (..),
    HasSrcPos (..),
    arrayifyType,
    arrayTypeView,
  )
where

import Prettyprinter
import Shape
import Text.Megaparsec.Pos (Pos, SourcePos (..), unPos)
import VName

-- | An annotation for source expressions whose type we haven't filled in
-- yet.
data Unchecked a = Unchecked
  deriving (Show, Eq, Functor)

instance Pretty (Unchecked a) where
  pretty Unchecked {} = ""

-- | An annotation for source expressions with a type (filled in during type
-- checking).
newtype Typed a = Typed a
  deriving (Show, Eq, Functor)

instance (Pretty a) => Pretty (Typed a) where
  pretty (Typed a) = pretty a

-- | Type variables.
data TVar v
  = -- | Atom-kinded type variable.
    AtomTVar v
  | -- | Array-kinded type variable.
    ArrayTVar v
  deriving (Show, Ord, Eq, Functor)

-- | Extract the variable out of a 'TVar'.
unTVar :: TVar v -> v
unTVar (AtomTVar v) = v
unTVar (ArrayTVar v) = v

instance (Show v, Pretty v) => Pretty (TVar v) where
  pretty (AtomTVar v) = "&" <> pretty v
  pretty (ArrayTVar v) = "*" <> pretty v

-- | Types.
data Type v
  = -- | Type variable.
    TVar (TVar v)
  | -- | Boolean type.
    Bool
  | -- | Integer type.
    Int
  | -- | Float type.
    Float
  | -- | Array type.
    TArr (Type v) (Shape v)
  | -- | Function type.
    (:->) [Type v] (Type v)
  | -- | Univerall type.
    Forall [TVar v] (Type v)
  | -- | Dependent product type.
    Prod [IVar v] (Type v)
  | -- | Dependent sum type.
    Exists [IVar v] (Type v)
  deriving (Show, Eq)

instance (Show v, Pretty v) => Pretty (Type v) where
  pretty (TVar x) = pretty x
  pretty Bool = "Bool"
  pretty Int = "Int"
  pretty Float = "Float"
  pretty (TArr t i) =
    parens $ "A" <+> pretty t <+> pretty i
  pretty (ts :-> t) =
    parens $ "->" <+> parens (hsep (map pretty ts)) <+> pretty t
  pretty (Forall xs t) =
    parens $
      "∀"
        <+> parens (hsep (map pretty xs))
        <+> pretty t
  pretty (Prod xs t) =
    parens $
      "Π"
        <+> parens (hsep (map pretty xs))
        <+> pretty t
  pretty (Exists xs t) =
    parens $
      "Σ"
        <+> parens (hsep (map pretty xs))
        <+> pretty t

-- | Get the element type.
elemType :: Type v -> Type v
elemType (TArr t _) = t
elemType t = t

-- | Does this type have Array kind?
arrayKind :: Type v -> Bool
arrayKind (TVar ArrayTVar {}) = True
arrayKind (TArr t _) = atomKind t
arrayKind _ = False

-- | Does this type have Atom kind?
atomKind :: Type v -> Bool
atomKind = not . arrayKind

-- | Base values.
data Base
  = BoolVal Bool
  | IntVal Int
  | FloatVal Float
  deriving (Show, Eq, Ord)

instance Pretty Base where
  pretty (BoolVal True) = "#t"
  pretty (BoolVal False) = "#f"
  pretty (IntVal i) = pretty i
  pretty (FloatVal f) = pretty f

-- | Atoms. An @Atom f v@ is an 'Atom' whose variables have type @v@ and with
-- type annotations of type @f (Type v)@. When @f = Unchecked@, the type
-- @Unchecked (Type v)@ only has a single inhabitant (namely 'Unchecked').
data Atom f v
  = -- | Base values.
    Base Base (f (Type v)) SourcePos
  | -- | Term lambda.
    Lambda [(v, Type v)] (Exp f v) (f (Type v)) SourcePos
  | -- | Type lambda.
    TLambda [TVar v] (Exp f v) (f (Type v)) SourcePos
  | -- | Index lambda.
    ILambda [IVar v] (Exp f v) (f (Type v)) SourcePos
  | -- | Boxed expression.
    Box [Idx v] (Exp f v) (Type v) SourcePos

deriving instance (Show v) => Show (Atom Unchecked v)

deriving instance (Show v) => Show (Atom Typed v)

instance (Show v, Pretty v, Pretty (f (Type v))) => Pretty (Atom f v) where
  pretty (Base b _ _) = pretty b
  pretty (Lambda args e _ _) =
    let pArgs =
          parens $
            hsep $
              map
                ( \(v, t) ->
                    pretty v <+> pretty t
                )
                args
     in parens $ "λ" <+> pArgs <+> pretty e
  pretty (TLambda args e _ _) =
    let pArgs =
          parens $
            hsep $
              map pretty args
     in parens $ "Tλ" <+> pArgs <+> pretty e
  pretty (ILambda args e _ _) =
    let pArgs =
          parens $
            hsep $
              map pretty args
     in parens $ "Iλ" <+> pArgs <+> pretty e
  pretty (Box is e t _) =
    parens $ "box" <+> hsep (map pretty is) <+> pretty e <+> pretty t

-- | Expressions.
data Exp f v
  = -- | Variables.
    Var v (f (Type v)) SourcePos
  | -- | Array literals.
    Array [Int] [Atom f v] (f (Type v)) SourcePos
  | -- | Empty arrays.
    EmptyArray [Int] (Type v) (f (Type v)) SourcePos
  | -- | Frame literals.
    Frame [Int] [Exp f v] (f (Type v)) SourcePos
  | -- | Empty frames.
    EmptyFrame [Int] (Type v) (f (Type v)) SourcePos
  | -- | Function application.
    App (Exp f v) [Exp f v] (f (Type v, Shape v)) SourcePos
  | -- | Type application.
    TApp (Exp f v) [Type v] (f (Type v)) SourcePos
  | -- | Index application.
    IApp (Exp f v) [Idx v] (f (Type v)) SourcePos
  | -- | Unboxing.
    Unbox [IVar v] v (Exp f v) (Exp f v) (f (Type v)) SourcePos

deriving instance (Show v) => Show (Exp Unchecked v)

deriving instance (Show v) => Show (Exp Typed v)

instance (Show v, Pretty v, Pretty (f (Type v))) => Pretty (Exp f v) where
  pretty (Var v _ _) = pretty v
  pretty (Array shape as _ _) =
    group $
      parens $
        "array"
          <+> parens (hsep (map pretty shape))
          <+> group (encloseSep "[" "]" line (map pretty as))
  pretty (EmptyArray shape t _ _) =
    group $
      parens $
        "empty-array"
          <+> parens (hsep (map pretty shape))
          <+> pretty t
  pretty (Frame shape es _ _) =
    group $
      parens $
        "frame"
          <+> parens (hsep (map pretty shape))
          <+> group (encloseSep "[" "]" line (map pretty es))
  pretty (EmptyFrame shape t _ _) =
    group $
      parens $
        "empty-frame"
          <+> parens (hsep (map pretty shape))
          <+> pretty t
  pretty (App f es _ _) =
    parens $ hsep $ map pretty (f : es)
  pretty (TApp e ts _ _) =
    parens $ "t-app" <+> pretty e <+> hsep (map pretty ts)
  pretty (IApp e is _ _) =
    parens $ "i-app" <+> pretty e <+> hsep (map pretty is)
  pretty (Unbox vs v e b _ _) =
    parens $ "unbox" <+> parens (hsep (map pretty vs ++ [pretty v, pretty e])) <+> pretty b

-- | Gets the 'Atom's of an 'Array' literal.
arrayElems :: Exp f v -> Maybe [Atom f v]
arrayElems (Array _ as _ _) = pure as
arrayElems _ = Nothing

-- | Gets the 'Exp's of a 'Frame' literal.
frameElems :: Exp f v -> Maybe [Exp f v]
frameElems (Frame _ es _ _) = pure es
frameElems _ = Nothing

-- | Flattens nested 'Frame's and 'Array's.
flattenExp :: Exp f v -> Exp f v
flattenExp (Frame shape es t pos) =
  case es' of
    (Frame shape' _ t' _ : _)
      | Just ess' <- mapM frameElems es' ->
          Frame (shape <> shape') (concat ess') t' pos
    (Array shape' _ t' _ : _)
      | Just ass' <- mapM arrayElems es' ->
          Array (shape <> shape') (concat ass') t' pos
    _ -> Frame shape es' t pos
  where
    es' = map flattenExp es
flattenExp e = e

-- | Things that have a type.
class HasType x where
  -- | Returns a normalized type.
  typeOf :: x -> Type VName
  typeOf = normType . typeOf_

  typeOf_ :: x -> Type VName

instance HasType Base where
  typeOf_ BoolVal {} = Bool
  typeOf_ IntVal {} = Int
  typeOf_ FloatVal {} = Float

instance HasType (Atom Typed VName) where
  typeOf_ (Base _ (Typed t) _) = t
  typeOf_ (Lambda _ _ (Typed t) _) = t
  typeOf_ (TLambda _ _ (Typed t) _) = t
  typeOf_ (ILambda _ _ (Typed t) _) = t
  typeOf_ (Box _ _ t _) = t

instance HasType (Exp Typed VName) where
  typeOf_ (Var _ (Typed t) _) = t
  typeOf_ (Array _ _ (Typed t) _) = t
  typeOf_ (EmptyArray _ _ (Typed t) _) = t
  typeOf_ (Frame _ _ (Typed t) _) = t
  typeOf_ (EmptyFrame _ _ (Typed t) _) = t
  typeOf_ (App _ _ (Typed (t, _)) _) = t
  typeOf_ (TApp _ _ (Typed t) _) = t
  typeOf_ (IApp _ _ (Typed t) _) = t
  typeOf_ (Unbox _ _ _ _ (Typed t) _) = t

-- | Normalizes a type by normalizing its constiuent shapes.
normType :: (Ord v, Eq v) => Type v -> Type v
normType (TArr t s) =
  let t'' =
        case normType t of
          TArr t' s' -> TArr t' (normShape s <> s')
          t' -> TArr t' (normShape s)
   in case t'' of
        TArr t''' (Concat []) -> t''' -- gross, fix
        _ -> t''
normType (ts :-> r) = map normType ts :-> normType r
normType (Forall pts t) = Forall pts $ normType t
normType (Prod pts t) = Prod pts $ normType t
normType (Exists pts t) = Exists pts $ normType t
normType t = t

-- | Things that have a shape.
class HasShape x where
  shapeOf :: x -> Shape VName
  shapeOf = normShape . shapeOf_

  shapeOf_ :: x -> Shape VName

instance HasShape (Atom f VName) where
  shapeOf_ _ = mempty

instance HasShape (Type VName) where
  shapeOf_ (TArr t s) = s <> shapeOf_ t
  shapeOf_ _ = mempty

instance
  (HasShape (Type VName), HasType (Exp f VName)) =>
  HasShape (Exp f VName)
  where
  shapeOf_ = shapeOf_ . typeOf_

-- | Things that have a source position.
class HasSrcPos x where
  posOf :: x -> SourcePos

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

arrayifyType :: Type VName -> Type VName
arrayifyType t@TArr {} = t
arrayifyType t = TArr t mempty

arrayTypeView :: Type v -> ((Type v, Shape v) -> a) -> a
arrayTypeView (TArr t s) m = m (t, s)
arrayTypeView t m = m (t, mempty)

instance Pretty Pos where
  pretty = pretty . unPos

instance Pretty SourcePos where
  pretty (SourcePos file l col) =
    pretty file <> ":" <> pretty l <> ":" <> pretty col
