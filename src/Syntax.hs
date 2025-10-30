{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Syntax
  ( module Shape,
    SourcePos,
    NoInfo (..),
    Info (..),
    TVar (..),
    unTVar,
    fromAtomTVar,
    fromArrayTVar,
    ScalarType (..),
    ArrayType (..),
    Type (..),
    mkScalarArrayType,
    elemType,
    atomKind,
    arrayKind,
    Base (..),
    Atom (..),
    Bind (..),
    Exp (..),
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

-- | No information functor; used as an annotation for source expressions whose
-- type we haven't filled in yet.
data NoInfo a = NoInfo
  deriving (Show, Eq, Functor)

instance Pretty (NoInfo a) where
  pretty NoInfo {} = ""

-- | Some information functor; used as an annotation for source expressions with
-- a type (filled in during type checking) and also a principal frame in the
-- case of applications.
newtype Info a = Info a
  deriving (Show, Eq, Functor)

instance (Pretty a) => Pretty (Info a) where
  pretty (Info a) = pretty a

-- | Type variables.
data TVar v
  = -- | Atom-kinded type variable.
    AtomTVar v
  | -- | Array-kinded type variable.
    ArrayTVar v
  deriving (Show, Ord, Eq, Functor, Foldable, Traversable)

-- | Extract the variable out of a 'TVar'.
unTVar :: TVar v -> v
unTVar (AtomTVar v) = v
unTVar (ArrayTVar v) = v

fromAtomTVar :: TVar v -> Maybe v
fromAtomTVar (AtomTVar v) = pure v
fromAtomTVar _ = Nothing

fromArrayTVar :: TVar v -> Maybe v
fromArrayTVar (ArrayTVar v) = pure v
fromArrayTVar _ = Nothing

instance (Show v, Pretty v) => Pretty (TVar v) where
  pretty (AtomTVar v) = "&" <> pretty v
  pretty (ArrayTVar v) = "*" <> pretty v

data ScalarType v
  = ScalarTVar v
  | -- | Boolean type.
    Bool
  | -- | Integer type.
    Int
  | -- | Float type.
    Float
  | -- | Function type.
    (:->) [ArrayType v] (ArrayType v)
  | -- | Univerall type.
    Forall [TVar v] (ArrayType v)
  | -- | Dependent product type.
    Pi [IVar v] (ArrayType v)
  | -- | Dependent sum type.
    Sigma [IVar v] (ArrayType v)
  deriving (Show, Eq)

instance (Show v, Pretty v) => Pretty (ScalarType v) where
  pretty (ScalarTVar x) = "&" <> pretty x
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
  = A
      { arrayTypeScalar :: (ScalarType v),
        arrayTypeShape :: (Shape v)
      }
  | ArrayTypeVar v
  deriving (Show, Eq)

instance (Show v, Pretty v) => Pretty (ArrayType v) where
  pretty (A t s) = parens $ "A" <+> pretty t <+> pretty s
  pretty (ArrayTypeVar x) = "*" <> pretty x

-- | Types.
data Type v
  = -- | Scalar types.
    ScalarType (ScalarType v)
  | -- | Array types.
    ArrayType (ArrayType v)
  deriving (Show, Eq)

instance (Show v, Pretty v) => Pretty (Type v) where
  pretty (ScalarType t) = pretty t
  pretty (ArrayType t) = pretty t

-- | Make a scalar array.
mkScalarArrayType :: ScalarType v -> ArrayType v
mkScalarArrayType = flip A mempty

-- | Get the element type.
elemType :: Type v -> ScalarType v
elemType (ScalarType t) = t
elemType (ArrayType (A t _)) = t

-- | Does this type have Array kind?
arrayKind :: Type v -> Bool
arrayKind ArrayType {} = True
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
-- type annotations of type @f (Type v)@. When @f = NoInfo@, the type
-- @NoInfo (Type v)@ only has a single inhabitant (namely 'NoInfo').
data Atom f v
  = -- | Base values.
    Base Base (f (ScalarType v)) SourcePos
  | -- | Term lambda.
    Lambda [(v, ArrayType v)] (Exp f v) (f (ScalarType v)) SourcePos
  | -- | Type lambda.
    TLambda [TVar v] (Exp f v) (f (ScalarType v)) SourcePos
  | -- | Index lambda.
    ILambda [IVar v] (Exp f v) (f (ScalarType v)) SourcePos
  | -- | Boxed expression.
    Box [Idx v] (Exp f v) (ScalarType v) SourcePos

deriving instance (Show v) => Show (Atom NoInfo v)

deriving instance (Show v) => Show (Atom Info v)

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
     in parens $ "tλ" <+> pArgs <+> pretty e
  pretty (ILambda args e _ _) =
    let pArgs =
          parens $
            hsep $
              map pretty args
     in parens $ "iλ" <+> pArgs <+> pretty e
  pretty (Box is e t _) =
    parens $ "box" <+> hsep (map pretty is) <+> pretty e <+> pretty t

-- | Binds
data Bind f v
  = BindVal v (ArrayType v) (Exp f v)
  | BindFun v [(v, (ArrayType v))] (ArrayType v) (Exp f v)
  | BindType (TVar v) (Type v)
  | BindIdx (IVar v) (Idx v)

deriving instance (Show v) => Show (Bind NoInfo v)

deriving instance (Show v) => Show (Bind Info v)

instance (Show v, Pretty v, Pretty (f (Type v))) => Pretty (Bind f v) where
  pretty (BindVal v t e) =
    parens $ pretty v <+> pretty t <+> pretty e
  pretty (BindFun f params t body) =
    parens $
      pretty f
        <+> parens (hsep (map (\(v, ty) -> parens $ pretty v <+> pretty ty) params))
        <+> pretty t
        <+> pretty body
  pretty (BindType tvar t) =
    parens $ pretty tvar <+> pretty t
  pretty (BindIdx ivar idx) =
    parens $ pretty ivar <+> pretty idx

-- | Expressions.
data Exp f v
  = -- | Variables.
    Var v (f (ArrayType v)) SourcePos
  | -- | Array literals.
    Array [Int] [Atom f v] (f (ArrayType v)) SourcePos
  | -- | Empty arrays.
    EmptyArray [Int] (ScalarType v) (f (ArrayType v)) SourcePos
  | -- | Frame literals.
    Frame [Int] [Exp f v] (f (ArrayType v)) SourcePos
  | -- | Empty frames.
    EmptyFrame [Int] (ScalarType v) (f (ArrayType v)) SourcePos
  | -- | Function application.
    App (Exp f v) [Exp f v] (f (ArrayType v, Shape v)) SourcePos
  | -- | Type application.
    TApp (Exp f v) [Type v] (f (ArrayType v)) SourcePos
  | -- | Index application.
    IApp (Exp f v) [Idx v] (f (ArrayType v)) SourcePos
  | -- | Unboxing.
    Unbox [IVar v] v (Exp f v) (Exp f v) (f (ArrayType v)) SourcePos
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
arrayifyType (ScalarType t) = ArrayType $ A t mempty
arrayifyType t@ArrayType {} = t

arrayTypeView :: Type v -> ((ScalarType v, Shape v) -> a) -> a
arrayTypeView (ArrayType (A t s)) m = m (t, s)
arrayTypeView (ScalarType t) m = m (t, mempty)

instance Pretty Pos where
  pretty = pretty . unPos

instance Pretty SourcePos where
  pretty (SourcePos file l col) =
    pretty file <> ":" <> pretty l <> ":" <> pretty col
