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
    bindName,
    declName,
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
    DeclBase (..),
    Decl,
    UncheckedDecl,
    UniqueDecl,
    ProgBase (..),
    Prog,
    Import (..),
    UncheckedProg,
    UniqueProg,
    HasSrcPos (..),
    mkScalar,
    asScalar,
    arrayElems,
    frameElems,
    flattenExp,
    arrayifyType,
    arrayTypeView,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
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
  deriving (Show, Eq, Ord)

infixr 4 :->

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

-- | An array type.
data ArrayType v
  = (:@)
  { arrayTypeAtom :: AtomType v,
    arrayTypeShape :: Shape v
  }
  deriving (Show, Eq, Ord)

infix 5 :@

instance (Show v, Pretty v) => Pretty (ArrayType v) where
  pretty (t :@ Concat []) = pretty t -- fix
  pretty (t :@ s) = brackets $ pretty t <+> pretty s

-- | Types.
data Type v
  = AtomType (AtomType v)
  | ArrayType (ArrayType v)
  deriving (Show, Eq, Ord)

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
mkScalarArrayType = flip (:@) mempty

nestedType :: (a -> ArrayType v -> AtomType v) -> NonEmpty a -> ArrayType v -> AtomType v
nestedType con (x :| []) r = con x r
nestedType con (x :| (y : xs)) r = con x $ mkScalarArrayType $ nestedType con (y :| xs) r

arrowType :: NonEmpty (ArrayType v) -> ArrayType v -> AtomType v
arrowType = nestedType (:->)

forallType :: NonEmpty (TypeParam v) -> ArrayType v -> AtomType v
forallType = nestedType Forall

piType :: NonEmpty (ISpaceParam v) -> ArrayType v -> AtomType v
piType = nestedType Pi

sigmaType :: NonEmpty (ISpaceParam v) -> ArrayType v -> AtomType v
sigmaType = nestedType Sigma

-- | Get the element type.
elemType :: Type v -> AtomType v
elemType (AtomType t) = t
elemType (ArrayType (t :@ _)) = t

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

data PatBase te f v
  = PatId v (te v) (f (ArrayType v)) SourcePos

patVar :: PatBase te f v -> v
patVar (PatId v _ _ _) = v

unpackPat :: PatBase te f v -> (v, te v)
unpackPat (PatId v t _ _) = (v, t)

deriving instance (Show v, Show (te v)) => Show (PatBase te NoInfo v)

deriving instance (Show v, Show (te v)) => Show (PatBase te Info v)

instance (Show v, Pretty v, Pretty (te v), Pretty (f (Type v))) => Pretty (PatBase te f v) where
  pretty (PatId v t _ _) = parens $ pretty v <+> pretty t

-- | Atoms. An @Atom f v@ is an 'Atom' whose variables have type @v@ and with
-- type annotations of type @f (Type v)@. When @f = NoInfo@, the type
-- @NoInfo (Type v)@ only has a single inhabitant (namely 'NoInfo').
data AtomBase te tp f v
  = -- | Base values.
    Base Base (f (AtomType v)) SourcePos
  | -- | Term lambda.
    Lambda (PatBase te f v) (ExpBase te tp f v) (f (AtomType v)) SourcePos
  | -- | Type lambda.
    TLambda (tp v) (ExpBase te tp f v) (f (AtomType v)) SourcePos
  | -- | Index lambda.
    ILambda (ISpaceParam v) (ExpBase te tp f v) (f (AtomType v)) SourcePos
  | -- | Boxed expression.
    Box (ISpace v) (ExpBase te tp f v) (te v) (f (AtomType v)) SourcePos

deriving instance (Show v, Show (te v), Show (tp v)) => Show (AtomBase te tp NoInfo v)

deriving instance (Show v, Show (te v), Show (tp v)) => Show (AtomBase te tp Info v)

instance
  ( Show v,
    Pretty v,
    Pretty
      (te v),
    Pretty (f (Type v)),
    Pretty (tp v)
  ) =>
  Pretty (AtomBase te tp f v)
  where
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
data BindBase te tp f v
  = BindVal v (Maybe (te v)) (ExpBase te tp f v) SourcePos
  | BindType (tp v) (te v) (f (Type v)) SourcePos
  | BindISpace (ISpaceParam v) (ISpace v) SourcePos
  | BindFun v (NonEmpty (PatBase te f v)) (Maybe (te v)) (ExpBase te tp f v) (f (AtomType v)) SourcePos
  | BindTFun v (NonEmpty (tp v)) (Maybe (te v)) (ExpBase te tp f v) (f (AtomType v)) SourcePos
  | BindIFun v (NonEmpty (ISpaceParam v)) (Maybe (te v)) (ExpBase te tp f v) (f (AtomType v)) SourcePos

deriving instance (Show v, Show (te v), Show (tp v)) => Show (BindBase te tp NoInfo v)

deriving instance (Show v, Show (te v), Show (tp v)) => Show (BindBase te tp Info v)

instance
  ( Show v,
    Pretty v,
    Pretty (te v),
    Pretty (f (Type v)),
    Pretty (tp v)
  ) =>
  Pretty (BindBase te tp f v)
  where
  pretty (BindVal v t e _) =
    parens $ pretty v <+> pretty t <+> pretty e
  pretty (BindFun f params mt body _ _) =
    parens $
      pretty f
        <+> parens (hsep (map pretty $ NE.toList params))
        <+> pretty mt
        <+> pretty body
  pretty (BindTFun f params mt body _ _) =
    parens $
      pretty f
        <+> parens (hsep (map pretty $ NE.toList params))
        <+> pretty mt
        <+> pretty body
  pretty (BindIFun f params mt body _ _) =
    parens $
      pretty f
        <+> parens (hsep (map pretty $ NE.toList params))
        <+> pretty mt
        <+> pretty body
  pretty (BindType tvar t _ _) =
    parens $ pretty tvar <+> pretty t
  pretty (BindISpace ivar ispace _) =
    parens $ pretty ivar <+> pretty ispace

bindName :: BindBase te tp f v -> Maybe v
bindName (BindVal v _ _ _) = Just v
bindName (BindFun v _ _ _ _ _) = Just v
bindName (BindTFun v _ _ _ _ _) = Just v
bindName (BindIFun v _ _ _ _ _) = Just v
bindName BindType {} = Nothing
bindName BindISpace {} = Nothing

-- | Expressions.
data ExpBase te tp f v
  = -- | Variables.
    Var v (f (ArrayType v)) SourcePos
  | -- | Array literals.
    Array [Int] (NE.NonEmpty (AtomBase te tp f v)) (f (ArrayType v)) SourcePos
  | -- | Empty arrays.
    EmptyArray [Int] (te v) (f (ArrayType v)) SourcePos
  | -- | Frame literals.
    Frame [Int] (NE.NonEmpty (ExpBase te tp f v)) (f (ArrayType v)) SourcePos
  | -- | Empty frames.
    EmptyFrame [Int] (te v) (f (ArrayType v)) SourcePos
  | -- | Function application.
    App (ExpBase te tp f v) (ExpBase te tp f v) (f (ArrayType v, Shape v)) SourcePos
  | -- | Type application.
    TApp (ExpBase te tp f v) (te v) (f (ArrayType v)) SourcePos
  | -- | Index application.
    IApp (ExpBase te tp f v) (ISpace v) (f (ArrayType v)) SourcePos
  | -- | Unboxing.
    Unbox (ISpaceParam v) v (ExpBase te tp f v) (ExpBase te tp f v) (f (ArrayType v)) SourcePos
  | -- | Let
    Let (NE.NonEmpty (BindBase te tp f v)) (ExpBase te tp f v) (f (ArrayType v)) SourcePos

deriving instance (Show v, Show (te v), Show (tp v)) => Show (ExpBase te tp NoInfo v)

deriving instance (Show v, Show (te v), Show (tp v)) => Show (ExpBase te tp Info v)

instance
  ( Show v,
    Pretty v,
    Pretty (te v),
    Pretty (f (Type v)),
    Pretty (tp v)
  ) =>
  Pretty (ExpBase te tp f v)
  where
  pretty (Var v _ _) = pretty v
  -- We should probably be more clever here and use Prop.@=, but I don't want to
  -- deal with the circular import right now.
  pretty (Array [] (a NE.:| []) _ _) =
    pretty a
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
  pretty (Frame [] (e NE.:| []) _ _) =
    pretty e
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

-- | Top-level declarations
data DeclBase te tp f v
  = -- | A top-level definition.
    Def (BindBase te tp f v)
  | -- | An entry point.
    Entry v [PatBase te f v] (Maybe (te v)) (ExpBase te tp f v) (f (AtomType v)) SourcePos

deriving instance (Show v, Show (te v), Show (tp v)) => Show (DeclBase te tp NoInfo v)

deriving instance (Show v, Show (te v), Show (tp v)) => Show (DeclBase te tp Info v)

instance
  ( Show v,
    Pretty v,
    Pretty (te v),
    Pretty (f (Type v)),
    Pretty (tp v)
  ) =>
  Pretty (DeclBase te tp f v)
  where
  pretty (Def b) =
    parens $ "def" <+> pretty b
  pretty (Entry f params mt body _ _) =
    parens $
      "entry"
        <+> pretty f
        <+> parens (hsep (map pretty params))
        <+> pretty mt
        <+> pretty body

declName :: DeclBase te tp f v -> Maybe v
declName (Def b) = bindName b
declName (Entry v _ _ _ _ _) = Just v

data Import = Import FilePath SourcePos
  deriving (Show, Eq, Ord)

instance Pretty Import where
  pretty (Import file _) = parens $ "import" <+> pretty file

-- | A Remora program.
data ProgBase te tp f v = Prog
  {progDecs :: [DeclBase te tp f v]}

deriving instance (Show v, Show (te v), Show (tp v)) => Show (ProgBase te tp NoInfo v)

deriving instance (Show v, Show (te v), Show (tp v)) => Show (ProgBase te tp Info v)

instance
  ( Show v,
    Pretty v,
    Pretty (te v),
    Pretty (f (Type v)),
    Pretty (tp v)
  ) =>
  Pretty (ProgBase te tp f v)
  where
  pretty = cat . punctuate line . map pretty . progDecs

-- | Make a scalar from an 'Atom'.
mkScalar :: AtomBase te tp NoInfo v -> ExpBase te tp NoInfo v
mkScalar a = Array [] (pure a) NoInfo $ posOf a

-- | Get the 'Atom' out of a scalar 'Array' literal.
asScalar :: ExpBase te tp f v -> Maybe (AtomBase te tp f v)
asScalar (Array [] (a NE.:| []) _ _) = Just a
asScalar _ = Nothing

-- | Gets the 'Atom's of an 'Array' literal.
arrayElems :: ExpBase te tp f v -> Maybe (NE.NonEmpty (AtomBase te tp f v))
arrayElems (Array _ as _ _) = pure as
arrayElems _ = Nothing

-- | Gets the 'Exp's of a 'Frame' literal.
frameElems :: ExpBase te tp f v -> Maybe (NE.NonEmpty (ExpBase te tp f v))
frameElems (Frame _ es _ _) = pure es
frameElems _ = Nothing

-- | Flattens nested 'Frame's and 'Array's.
flattenExp :: ExpBase te tp f v -> ExpBase te tp f v
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
arrayifyType (AtomType t) = ArrayType $ t :@ mempty
arrayifyType t@ArrayType {} = t

arrayTypeView :: Type v -> ((AtomType v, Shape v) -> a) -> a
arrayTypeView (ArrayType (t :@ s)) m = m (t, s)
arrayTypeView (AtomType t) m = m (t, mempty)

instance Pretty Pos where
  pretty = pretty . unPos

instance Pretty SourcePos where
  pretty (SourcePos file l col) =
    pretty file <> ":" <> pretty l <> ":" <> pretty col

-- | Things that have a source position.
class HasSrcPos x where
  posOf :: x -> SourcePos

instance HasSrcPos (BindBase te tp f v) where
  posOf (BindVal _ _ _ pos) = pos
  posOf (BindFun _ _ _ _ _ pos) = pos
  posOf (BindTFun _ _ _ _ _ pos) = pos
  posOf (BindIFun _ _ _ _ _ pos) = pos
  posOf (BindType _ _ _ pos) = pos
  posOf (BindISpace _ _ pos) = pos

instance HasSrcPos (AtomBase te tp f v) where
  posOf (Base _ _ pos) = pos
  posOf (Lambda _ _ _ pos) = pos
  posOf (TLambda _ _ _ pos) = pos
  posOf (ILambda _ _ _ pos) = pos
  posOf (Box _ _ _ _ pos) = pos

instance HasSrcPos (ExpBase te tp f v) where
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

instance HasSrcPos (PatBase te f v) where
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

type UncheckedProg = ProgBase TypeExp TypeParamExp NoInfo Text

type UncheckedDecl = DeclBase TypeExp TypeParamExp NoInfo Text

type UncheckedExp = ExpBase TypeExp TypeParamExp NoInfo Text

type UncheckedAtom = AtomBase TypeExp TypeParamExp NoInfo Text

type UncheckedBind = BindBase TypeExp TypeParamExp NoInfo Text

type UncheckedPat = PatBase TypeExp NoInfo Text

type UniqueProg = ProgBase TypeExp TypeParam NoInfo VName

type UniqueDecl = DeclBase TypeExp TypeParam NoInfo VName

type UniqueExp = ExpBase TypeExp TypeParam NoInfo VName

type UniqueAtom = AtomBase TypeExp TypeParam NoInfo VName

type UniqueBind = BindBase TypeExp TypeParam NoInfo VName

type UniquePat = PatBase TypeExp NoInfo VName

type Prog = ProgBase Type TypeParam Info VName

type Decl = DeclBase Type TypeParam Info VName

type Exp = ExpBase Type TypeParam Info VName

type Atom = AtomBase Type TypeParam Info VName

type Bind = BindBase Type TypeParam Info VName

type Pat = PatBase Type Info VName
