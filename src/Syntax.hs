{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Syntax
  ( module Shape,
    SourcePos,
    NoInfo (..),
    Info (..),
    TypeParam (..),
    TypeExp (..),
    unTypeParam,
    fromAtomTypeParam,
    fromArrayTypeParam,
    AtomType (..),
    ArrayType (..),
    Type (..),
    fromAtomType,
    fromArrayType,
    mkScalarArrayType,
    elemType,
    Base (..),
    Pat (..),
    patVar,
    unpackPat,
    Atom (..),
    Bind (..),
    Exp (..),
    HasSrcPos (..),
    mkScalar,
    arrayElems,
    frameElems,
    flattenExp,
    arrayifyType,
    arrayTypeView,
  )
where

import Prettyprinter
import Shape
import Text.Megaparsec.Pos (Pos, SourcePos (..), unPos)
import VName

-- | No information functor; used as an annotation for parsed source expressions
-- that haven't been annotated with information from typechecking yet.
data NoInfo a = NoInfo
  deriving (Show, Eq, Functor)

instance Pretty (NoInfo a) where
  pretty NoInfo {} = ""

-- | Some information functor; used as an annotation for source expressions.
newtype Info a = Info a
  deriving (Show, Eq, Functor)

instance (Pretty a) => Pretty (Info a) where
  pretty (Info a) = pretty a

-- | Type parameters.
data TypeParam v
  = AtomTypeParam v
  | ArrayTypeParam v
  deriving (Show, Ord, Eq, Functor, Foldable, Traversable)

-- | Source-level types.
data TypeExp v
  = TEAtomVar v SourcePos
  | TEArrayVar v SourcePos
  | TEBool SourcePos
  | TEInt SourcePos
  | TEFloat SourcePos
  | TEArray (TypeExp v) (Shape v) SourcePos
  | TEArrow [TypeExp v] (TypeExp v) SourcePos
  | TEForall [TypeParam v] (TypeExp v) SourcePos
  | TEPi [ExtentParam v] (TypeExp v) SourcePos
  | TESigma [ExtentParam v] (TypeExp v) SourcePos
  deriving (Eq, Ord, Show)

instance (Show v, Pretty v) => Pretty (TypeExp v) where
  pretty (TEAtomVar v _) = "&" <> pretty v
  pretty (TEArrayVar v _) = "*" <> pretty v
  pretty (TEBool _) = "Bool"
  pretty (TEInt _) = "Int"
  pretty (TEFloat _) = "Float"
  pretty (TEArray t s _) =
    parens $ "A" <+> pretty t <+> pretty s
  pretty (TEArrow ts t _) =
    parens $ "->" <+> parens (hsep (map pretty ts)) <+> pretty t
  pretty (TEForall xs t _) =
    parens $
      "∀"
        <+> parens (hsep (map pretty xs))
        <+> pretty t
  pretty (TEPi xs t _) =
    parens $
      "Π"
        <+> parens (hsep (map pretty xs))
        <+> pretty t
  pretty (TESigma xs t _) =
    parens $
      "Σ"
        <+> parens (hsep (map pretty xs))
        <+> pretty t

-- | Extract the variable out of a 'TypeParam'.
unTypeParam :: TypeParam v -> v
unTypeParam (AtomTypeParam v) = v
unTypeParam (ArrayTypeParam v) = v

fromAtomTypeParam :: TypeParam v -> Maybe v
fromAtomTypeParam (AtomTypeParam v) = pure v
fromAtomTypeParam _ = Nothing

fromArrayTypeParam :: TypeParam v -> Maybe v
fromArrayTypeParam (ArrayTypeParam v) = pure v
fromArrayTypeParam _ = Nothing

instance (Show v, Pretty v) => Pretty (TypeParam v) where
  pretty (AtomTypeParam v) = "&" <> pretty v
  pretty (ArrayTypeParam v) = "*" <> pretty v

data AtomType v
  = -- | Atom type variable.
    AtomTypeVar v
  | -- | Boolean type.
    Bool
  | -- | Integer type.
    Int
  | -- | Float type.
    Float
  | -- | Function type.
    (:->) [ArrayType v] (ArrayType v)
  | -- | Univerall type.
    Forall [TypeParam v] (ArrayType v)
  | -- | Dependent product type.
    Pi [ExtentParam v] (ArrayType v)
  | -- | Dependent sum type.
    Sigma [ExtentParam v] (ArrayType v)
  deriving (Show, Eq)

instance (Show v, Pretty v) => Pretty (AtomType v) where
  pretty (AtomTypeVar x) = "&" <> pretty x
  pretty Bool = "Bool"
  pretty Int = "Int"
  pretty Float = "Float"
  pretty (ts :-> t) =
    parens $ "->" <+> parens (hsep (map pretty ts)) <+> pretty t
  pretty (Forall xs t) =
    parens $
      "∀"
        <+> parens (hsep (map pretty xs))
        <+> pretty t
  pretty (Pi xs t) =
    parens $
      "Π"
        <+> parens (hsep (map pretty xs))
        <+> pretty t
  pretty (Sigma xs t) =
    parens $
      "Σ"
        <+> parens (hsep (map pretty xs))
        <+> pretty t

data ArrayType v
  = -- | An array type literal consisting of an atom type and a shape.
    A
    { arrayTypeAtom :: AtomType v,
      arrayTypeShape :: Shape v
    }
  deriving (Eq, Show)

instance (Show v, Pretty v) => Pretty (ArrayType v) where
  pretty (A t s) = parens $ "A" <+> pretty t <+> pretty s

-- | Types.
data Type v
  = AtomType (AtomType v)
  | ArrayType (ArrayType v)
  deriving (Eq, Show)

instance (Show v, Pretty v) => Pretty (Type v) where
  pretty (AtomType t) = pretty t
  pretty (ArrayType t) = pretty t

fromAtomType :: Type v -> Maybe (AtomType v)
fromAtomType (AtomType t) = pure t
fromAtomType _ = Nothing

fromArrayType :: Type v -> Maybe (ArrayType v)
fromArrayType (ArrayType t) = pure t
fromArrayType _ = Nothing

-- | Make a scalar array.
mkScalarArrayType :: AtomType v -> ArrayType v
mkScalarArrayType = flip A mempty

-- | Get the element type.
elemType :: Type v -> AtomType v
elemType (AtomType t) = t
elemType (ArrayType (A t _)) = t

-- | Base values.
data Base
  = BoolVal Bool
  | IntVal Integer
  | FloatVal Float
  deriving (Show, Eq, Ord)

instance Pretty Base where
  pretty (BoolVal True) = "#t"
  pretty (BoolVal False) = "#f"
  pretty (IntVal i) = pretty i
  pretty (FloatVal f) = pretty f

data Pat f v
  = PatId v (TypeExp v) (f (ArrayType v)) SourcePos

patVar :: Pat f v -> v
patVar (PatId v _ _ _) = v

unpackPat :: Pat f v -> (v, TypeExp v)
unpackPat (PatId v t _ _) = (v, t)

deriving instance (Show v) => Show (Pat NoInfo v)

deriving instance (Show v) => Show (Pat Info v)

instance (Show v, Pretty v, Pretty (f (Type v))) => Pretty (Pat f v) where
  pretty (PatId v t _ _) = parens $ pretty v <+> pretty t

-- | Atoms. An @Atom f v@ is an 'Atom' whose variables have type @v@ and with
-- type annotations of type @f (Type v)@. When @f = NoInfo@, the type
-- @NoInfo (Type v)@ only has a single inhabitant (namely 'NoInfo').
data Atom f v
  = -- | Base values.
    Base Base (f (AtomType v)) SourcePos
  | -- | Term lambda.
    Lambda [Pat f v] (Exp f v) (f (AtomType v)) SourcePos
  | -- | Type lambda.
    TLambda [TypeParam v] (Exp f v) (f (AtomType v)) SourcePos
  | -- | Index lambda.
    ILambda [ExtentParam v] (Exp f v) (f (AtomType v)) SourcePos
  | -- | Boxed expression.
    Box [Extent v] (Exp f v) (TypeExp v) (f (AtomType v)) SourcePos

deriving instance (Show v) => Show (Atom NoInfo v)

deriving instance (Show v) => Show (Atom Info v)

instance (Show v, Pretty v, Pretty (f (Type v))) => Pretty (Atom f v) where
  pretty (Base b _ _) = pretty b
  pretty (Lambda args e _ _) =
    let pArgs =
          parens $
            hsep $
              map pretty args
     in parens $ "λ" <+> pArgs <+> pretty e
  pretty (TLambda args e _ _) =
    let pArgs =
          parens $
            hsep $
              map pretty args
     in parens $ "tλ" <+> pArgs <+> pretty e
  pretty (ILambda args e _ _) =
    let pArgs =
          parens $
            hsep $
              map pretty args
     in parens $ "iλ" <+> pArgs <+> pretty e
  pretty (Box is e t _ _) =
    parens $ "box" <+> hsep (map pretty is) <+> pretty e <+> pretty t

-- | Binds
data Bind f v
  = BindVal v (Maybe (TypeExp v)) (Exp f v) SourcePos
  | BindType (TypeParam v) (TypeExp v) (f (Type v)) SourcePos
  | BindExtent (ExtentParam v) (Extent v) SourcePos
  | BindFun v [Pat f v] (Maybe (TypeExp v)) (Exp f v) (f (AtomType v)) SourcePos
  | BindTFun v [TypeParam v] (Maybe (TypeExp v)) (Exp f v) (f (AtomType v)) SourcePos
  | BindIFun v [ExtentParam v] (Maybe (TypeExp v)) (Exp f v) (f (AtomType v)) SourcePos

deriving instance (Show v) => Show (Bind NoInfo v)

deriving instance (Show v) => Show (Bind Info v)

instance (Show v, Pretty v, Pretty (f (Type v))) => Pretty (Bind f v) where
  pretty (BindVal v t e _) =
    parens $ pretty v <+> pretty t <+> pretty e
  pretty (BindFun f params mt body _ _) =
    parens $
      pretty f
        <+> parens (hsep (map pretty params))
        <+> pretty mt
        <+> pretty body
  pretty (BindTFun f params mt body _ _) =
    parens $
      pretty f
        <+> parens (hsep (map pretty params))
        <+> pretty mt
        <+> pretty body
  pretty (BindIFun f params mt body _ _) =
    parens $
      pretty f
        <+> parens (hsep (map pretty params))
        <+> pretty mt
        <+> pretty body
  pretty (BindType tvar t _ _) =
    parens $ pretty tvar <+> pretty t
  pretty (BindExtent ivar extent _) =
    parens $ pretty ivar <+> pretty extent

-- | Expressions.
data Exp f v
  = -- | Variables.
    Var v (f (ArrayType v)) SourcePos
  | -- | Array literals.
    Array [Int] [Atom f v] (f (ArrayType v)) SourcePos
  | -- | Empty arrays.
    EmptyArray [Int] (TypeExp v) (f (ArrayType v)) SourcePos
  | -- | Frame literals.
    Frame [Int] [Exp f v] (f (ArrayType v)) SourcePos
  | -- | Empty frames.
    EmptyFrame [Int] (TypeExp v) (f (ArrayType v)) SourcePos
  | -- | Function application.
    App (Exp f v) [Exp f v] (f (ArrayType v, Shape v)) SourcePos
  | -- | Type application.
    TApp (Exp f v) [TypeExp v] (f (ArrayType v)) SourcePos
  | -- | Index application.
    IApp (Exp f v) [Extent v] (f (ArrayType v)) SourcePos
  | -- | Unboxing.
    Unbox [ExtentParam v] v (Exp f v) (Exp f v) (f (ArrayType v)) SourcePos
  | -- | Let
    Let [Bind f v] (Exp f v) (f (ArrayType v)) SourcePos

deriving instance (Show v) => Show (Exp NoInfo v)

deriving instance (Show v) => Show (Exp Info v)

instance (Show v, Pretty v, Pretty (f (Type v))) => Pretty (Exp f v) where
  pretty (Var v _ _) = pretty v
  pretty (Array shape as _ _) =
    group $
      parens $
        "array"
          <+> parens (hsep (map pretty shape))
          <+> group (brackets (hsep (map pretty as)))
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
          <+> group (brackets (hsep (map pretty es)))
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
  pretty (Let binds body _ _) =
    parens $ "let" <+> hsep (map pretty binds) <+> pretty body

-- | Make a scalar from an 'Atom'.
mkScalar :: Atom NoInfo v -> Exp NoInfo v
mkScalar a = Array [1] [a] NoInfo $ posOf a

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

arrayifyType :: Type VName -> Type VName
arrayifyType (AtomType t) = ArrayType $ A t mempty
arrayifyType t@ArrayType {} = t

arrayTypeView :: Type v -> ((AtomType v, Shape v) -> a) -> a
arrayTypeView (ArrayType (A t s)) m = m (t, s)
arrayTypeView (AtomType t) m = m (t, mempty)

instance Pretty Pos where
  pretty = pretty . unPos

instance Pretty SourcePos where
  pretty (SourcePos file l col) =
    pretty file <> ":" <> pretty l <> ":" <> pretty col

-- | Things that have a source position.
class HasSrcPos x where
  posOf :: x -> SourcePos

instance HasSrcPos (Bind f v) where
  posOf (BindVal _ _ _ pos) = pos
  posOf (BindFun _ _ _ _ _ pos) = pos
  posOf (BindType _ _ _ pos) = pos
  posOf (BindExtent _ _ pos) = pos

instance HasSrcPos (Atom f v) where
  posOf (Base _ _ pos) = pos
  posOf (Lambda _ _ _ pos) = pos
  posOf (TLambda _ _ _ pos) = pos
  posOf (ILambda _ _ _ pos) = pos
  posOf (Box _ _ _ _ pos) = pos

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

instance HasSrcPos (Pat f v) where
  posOf (PatId _ _ _ pos) = pos

instance HasSrcPos (TypeExp v) where
  posOf (TEAtomVar _ pos) = pos
  posOf (TEArrayVar _ pos) = pos
  posOf (TEBool pos) = pos
  posOf (TEInt pos) = pos
  posOf (TEFloat pos) = pos
  posOf (TEArray _ _ pos) = pos
  posOf (TEArrow _ _ pos) = pos
  posOf (TEForall _ _ pos) = pos
  posOf (TEPi _ _ pos) = pos
  posOf (TESigma _ _ pos) = pos
