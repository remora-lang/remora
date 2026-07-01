{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Syntax
  ( module ISpace,
    SourcePos,
    NoInfo (..),
    Info (..),
    TypeParamExp (..),
    TypeExp (..),
    unTypeParam,
    TypeParam (..),
    AtomType (..),
    ArrayType (..),
    Type (..),
    fromAtomType,
    fromArrayType,
    mkScalarArrayType,
    arrowType,
    forallType,
    piType,
    sigmaType,
    elemType,
    Base (..),
    PatBase (..),
    Pat,
    UncheckedPat,
    UniquePat,
    patVar,
    unpackPat,
    AtomBase (..),
    Atom,
    UncheckedAtom,
    UniqueAtom,
    BindBase (..),
    Bind,
    UncheckedBind,
    UniqueBind,
    ExpBase (..),
    Exp,
    UncheckedExp,
    UniqueExp,
    HasSrcPos (..),
    mkScalar,
    arrayElems,
    frameElems,
    flattenExp,
    arrayifyType,
    arrayTypeView,
  )
where

import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import ISpace
import Prettyprinter
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

-- | Source-level type parameters. Different from `TypeParam` because they allow
-- array type parameters.
data TypeParamExp v
  = TEAtomTypeParam v
  | TEArrayTypeParam v
  deriving (Show, Ord, Eq, Functor, Foldable, Traversable)

instance (Show v, Pretty v) => Pretty (TypeParamExp v) where
  pretty (TEAtomTypeParam v) = "&" <> pretty v
  pretty (TEArrayTypeParam v) = "*" <> pretty v

-- | Source-level types.
data TypeExp v
  = TEAtomVar v SourcePos
  | TEArrayVar v SourcePos
  | TEBool SourcePos
  | TEInt SourcePos
  | TEFloat SourcePos
  | TEArray (TypeExp v) (Shape v) SourcePos
  | TEArrow (TypeExp v) (TypeExp v) SourcePos
  | TEForall (NE.NonEmpty (TypeParamExp v)) (TypeExp v) SourcePos
  | TEPi (NE.NonEmpty (ISpaceParam v)) (TypeExp v) SourcePos
  | TESigma (NE.NonEmpty (ISpaceParam v)) (TypeExp v) SourcePos
  deriving (Eq, Ord, Show)

instance (Show v, Pretty v) => Pretty (TypeExp v) where
  pretty (TEAtomVar v _) = "&" <> pretty v
  pretty (TEArrayVar v _) = "*" <> pretty v
  pretty (TEBool _) = "Bool"
  pretty (TEInt _) = "Int"
  pretty (TEFloat _) = "Float"
  pretty (TEArray t s _) =
    parens $ "A" <+> pretty t <+> pretty s
  pretty (TEArrow t1 t2 _) =
    parens $ "->" <+> pretty t1 <+> pretty t2
  pretty (TEForall xs t _) =
    parens $
      "∀"
        <+> parens (hsep (map pretty (NE.toList xs)))
        <+> pretty t
  pretty (TEPi xs t _) =
    parens $
      "Π"
        <+> parens (hsep (map pretty (NE.toList xs)))
        <+> pretty t
  pretty (TESigma xs t _) =
    parens $
      "Σ"
        <+> parens (hsep (map pretty (NE.toList xs)))
        <+> pretty t

-- | Type parameters.
newtype TypeParam v = AtomTypeParam v
  deriving (Show, Ord, Eq, Functor, Foldable, Traversable)

instance (Show v, Pretty v) => Pretty (TypeParam v) where
  pretty (AtomTypeParam v) = "&" <> pretty v

-- | Extract the variable out of a 'TypeParam'.
unTypeParam :: TypeParam v -> v
unTypeParam (AtomTypeParam v) = v

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
    (:->) (ArrayType v) (ArrayType v)
  | -- | Univerall type.
    Forall (TypeParam v) (ArrayType v)
  | -- | Dependent product type.
    Pi (ISpaceParam v) (ArrayType v)
  | -- | Dependent sum type.
    Sigma (ISpaceParam v) (ArrayType v)
  deriving (Show, Eq)

instance (Show v, Pretty v) => Pretty (AtomType v) where
  pretty (AtomTypeVar x) = "&" <> pretty x
  pretty Bool = "Bool"
  pretty Int = "Int"
  pretty Float = "Float"
  pretty (t1 :-> t2) =
    parens $ "->" <+> pretty t1 <+> pretty t2
  pretty (Forall x t) =
    parens $
      "∀"
        <+> pretty x
        <+> pretty t
  pretty (Pi x t) =
    parens $
      "Π"
        <+> pretty x
        <+> pretty t
  pretty (Sigma x t) =
    parens $
      "Σ"
        <+> pretty x
        <+> pretty t

-- | An array type literal consisting of an atom type and a shape.
data ArrayType v
  = A
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

nestedType :: (a -> ArrayType v -> AtomType v) -> [a] -> ArrayType v -> AtomType v
nestedType _ [] r = arrayTypeAtom r
nestedType con [x] r = con x r
nestedType con (x : xs) r = con x $ mkScalarArrayType $ nestedType con xs r

arrowType :: [ArrayType v] -> ArrayType v -> AtomType v
arrowType = nestedType (:->)

forallType :: [TypeParam v] -> ArrayType v -> AtomType v
forallType = nestedType Forall

piType :: [ISpaceParam v] -> ArrayType v -> AtomType v
piType = nestedType Pi

sigmaType :: [ISpaceParam v] -> ArrayType v -> AtomType v
sigmaType = nestedType Sigma

-- | Get the element type.
elemType :: Type v -> AtomType v
elemType (AtomType t) = t
elemType (ArrayType (A t _)) = t

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

data PatBase f v
  = PatId v (TypeExp v) (f (ArrayType v)) SourcePos

patVar :: PatBase f v -> v
patVar (PatId v _ _ _) = v

unpackPat :: PatBase f v -> (v, TypeExp v)
unpackPat (PatId v t _ _) = (v, t)

deriving instance (Show v) => Show (PatBase NoInfo v)

deriving instance (Show v) => Show (PatBase Info v)

instance (Show v, Pretty v, Pretty (f (Type v))) => Pretty (PatBase f v) where
  pretty (PatId v t _ _) = parens $ pretty v <+> pretty t

-- | Atoms. An @Atom f v@ is an 'Atom' whose variables have type @v@ and with
-- type annotations of type @f (Type v)@. When @f = NoInfo@, the type
-- @NoInfo (Type v)@ only has a single inhabitant (namely 'NoInfo').
data AtomBase tp f v
  = -- | Base values.
    Base Base (f (AtomType v)) SourcePos
  | -- | Term lambda.
    Lambda (PatBase f v) (ExpBase tp f v) (f (AtomType v)) SourcePos
  | -- | Type lambda.
    TLambda (tp v) (ExpBase tp f v) (f (AtomType v)) SourcePos
  | -- | Index lambda.
    ILambda (ISpaceParam v) (ExpBase tp f v) (f (AtomType v)) SourcePos
  | -- | Boxed expression.
    Box (ISpace v) (ExpBase tp f v) (TypeExp v) (f (AtomType v)) SourcePos

deriving instance (Show v, Show (tp v)) => Show (AtomBase tp NoInfo v)

deriving instance (Show v, Show (tp v)) => Show (AtomBase tp Info v)

instance (Show v, Pretty v, Pretty (f (Type v)), Pretty (tp v)) => Pretty (AtomBase tp f v) where
  pretty (Base b _ _) = pretty b
  pretty (Lambda arg e _ _) =
    parens $ "λ" <+> parens (pretty arg) <+> pretty e
  pretty (TLambda arg e _ _) =
    parens $ "tλ" <+> parens (pretty arg) <+> pretty e
  pretty (ILambda arg e _ _) =
    parens $ "iλ" <+> parens (pretty arg) <+> pretty e
  pretty (Box i e t _ _) =
    parens $ "box" <+> pretty i <+> pretty e <+> pretty t

-- | Binds
data BindBase tp f v
  = BindVal v (Maybe (TypeExp v)) (ExpBase tp f v) SourcePos
  | BindType (tp v) (TypeExp v) (f (Type v)) SourcePos
  | BindISpace (ISpaceParam v) (ISpace v) SourcePos
  | BindFun v [PatBase f v] (Maybe (TypeExp v)) (ExpBase tp f v) (f (AtomType v)) SourcePos
  | BindTFun v [tp v] (Maybe (TypeExp v)) (ExpBase tp f v) (f (AtomType v)) SourcePos
  | BindIFun v [ISpaceParam v] (Maybe (TypeExp v)) (ExpBase tp f v) (f (AtomType v)) SourcePos

deriving instance (Show v, Show (tp v)) => Show (BindBase tp NoInfo v)

deriving instance (Show v, Show (tp v)) => Show (BindBase tp Info v)

instance (Show v, Pretty v, Pretty (f (Type v)), Pretty (tp v)) => Pretty (BindBase tp f v) where
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
  pretty (BindISpace ivar ispace _) =
    parens $ pretty ivar <+> pretty ispace

-- | Expressions.
data ExpBase tp f v
  = -- | Variables.
    Var v (f (ArrayType v)) SourcePos
  | -- | Array literals.
    Array [Int] (NE.NonEmpty (AtomBase tp f v)) (f (ArrayType v)) SourcePos
  | -- | Empty arrays.
    EmptyArray [Int] (TypeExp v) (f (ArrayType v)) SourcePos
  | -- | Frame literals.
    Frame [Int] (NE.NonEmpty (ExpBase tp f v)) (f (ArrayType v)) SourcePos
  | -- | Empty frames.
    EmptyFrame [Int] (TypeExp v) (f (ArrayType v)) SourcePos
  | -- | Function application.
    App (ExpBase tp f v) (ExpBase tp f v) (f (ArrayType v, Shape v)) SourcePos
  | -- | Type application.
    TApp (ExpBase tp f v) (TypeExp v) (f (ArrayType v)) SourcePos
  | -- | Index application.
    IApp (ExpBase tp f v) (ISpace v) (f (ArrayType v)) SourcePos
  | -- | Unboxing.
    Unbox (ISpaceParam v) v (ExpBase tp f v) (ExpBase tp f v) (f (ArrayType v)) SourcePos
  | -- | Let
    Let (NE.NonEmpty (BindBase tp f v)) (ExpBase tp f v) (f (ArrayType v)) SourcePos

deriving instance (Show v, Show (tp v)) => Show (ExpBase tp NoInfo v)

deriving instance (Show v, Show (tp v)) => Show (ExpBase tp Info v)

instance (Show v, Pretty v, Pretty (f (Type v)), Pretty (tp v)) => Pretty (ExpBase tp f v) where
  pretty (Var v _ _) = pretty v
  pretty (Array shape as _ _) =
    group $
      parens $
        "array"
          <+> brackets (hsep (map pretty shape))
          <+> group (hsep (map pretty $ NE.toList as))
  pretty (EmptyArray shape t _ _) =
    group $
      parens $
        "empty-array"
          <+> brackets (hsep (map pretty shape))
          <+> pretty t
  pretty (Frame shape es _ _) =
    group $
      parens $
        "frame"
          <+> brackets (hsep (map pretty shape))
          <+> group (hsep (map pretty $ NE.toList es))
  pretty (EmptyFrame shape t _ _) =
    group $
      parens $
        "empty-frame"
          <+> brackets (hsep (map pretty shape))
          <+> pretty t
  pretty (App f e _ _) =
    parens $ pretty f <+> pretty e
  pretty (TApp e t _ _) =
    parens $ "t-app" <+> pretty e <+> pretty t
  pretty (IApp e i _ _) =
    parens $ "i-app" <+> pretty e <+> pretty i
  pretty (Unbox ep v e b _ _) =
    parens $ "unbox" <+> parens (pretty ep <+> pretty v <+> pretty e) <+> pretty b
  pretty (Let binds body _ _) =
    parens $ "let" <+> hsep (map pretty $ NE.toList binds) <+> pretty body

-- | Make a scalar from an 'Atom'.
mkScalar :: AtomBase tp NoInfo v -> ExpBase tp NoInfo v
mkScalar a = Array [] (pure a) NoInfo $ posOf a

-- | Gets the 'Atom's of an 'Array' literal.
arrayElems :: ExpBase tp f v -> Maybe (NE.NonEmpty (AtomBase tp f v))
arrayElems (Array _ as _ _) = pure as
arrayElems _ = Nothing

-- | Gets the 'Exp's of a 'Frame' literal.
frameElems :: ExpBase tp f v -> Maybe (NE.NonEmpty (ExpBase tp f v))
frameElems (Frame _ es _ _) = pure es
frameElems _ = Nothing

-- | Flattens nested 'Frame's and 'Array's.
flattenExp :: ExpBase tp f v -> ExpBase tp f v
flattenExp (Frame shape es t pos) =
  case NE.head es' of
    Frame shape' _ _ _
      | Just ess' <- traverse frameElems es' ->
          Frame (shape <> shape') (foldr1 (<>) ess') t pos
    Array shape' _ _ _
      | Just ass' <- traverse arrayElems es' ->
          Array (shape <> shape') (foldr1 (<>) ass') t pos
    _ -> Frame shape es' t pos
  where
    es' = fmap flattenExp es
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

instance HasSrcPos (BindBase tp f v) where
  posOf (BindVal _ _ _ pos) = pos
  posOf (BindFun _ _ _ _ _ pos) = pos
  posOf (BindTFun _ _ _ _ _ pos) = pos
  posOf (BindIFun _ _ _ _ _ pos) = pos
  posOf (BindType _ _ _ pos) = pos
  posOf (BindISpace _ _ pos) = pos

instance HasSrcPos (AtomBase tp f v) where
  posOf (Base _ _ pos) = pos
  posOf (Lambda _ _ _ pos) = pos
  posOf (TLambda _ _ _ pos) = pos
  posOf (ILambda _ _ _ pos) = pos
  posOf (Box _ _ _ _ pos) = pos

instance HasSrcPos (ExpBase tp f v) where
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

instance HasSrcPos (PatBase f v) where
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

type UncheckedExp = ExpBase TypeParamExp NoInfo Text

type UncheckedAtom = AtomBase TypeParamExp NoInfo Text

type UncheckedBind = BindBase TypeParamExp NoInfo Text

type UncheckedPat = PatBase NoInfo Text

type UniqueExp = ExpBase TypeParam NoInfo VName

type UniqueAtom = AtomBase TypeParam NoInfo VName

type UniqueBind = BindBase TypeParam NoInfo VName

type UniquePat = PatBase NoInfo VName

type Exp = ExpBase TypeParam Info VName

type Atom = AtomBase TypeParam Info VName

type Bind = BindBase TypeParam Info VName

type Pat = PatBase Info VName
