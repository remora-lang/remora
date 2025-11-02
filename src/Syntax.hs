{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Syntax
  ( module Shape,
    SourcePos,
    NoInfo (..),
    Info (..),
    TypeParam (..),
    unTypeParam,
    fromAtomTypeParam,
    fromArrayTypeParam,
    AtomType (..),
    ArrayType (..),
    Type (..),
    mkScalarArrayType,
    elemType,
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

data AtomType f v
  = -- | Atom type variable.
    AtomTypeVar v
  | -- | Boolean type.
    Bool
  | -- | Integer type.
    Int
  | -- | Float type.
    Float
  | -- | Function type.
    (:->) [ArrayType f v] (ArrayType f v)
  | -- | Univerall type.
    Forall [TypeParam v] (ArrayType f v)
  | -- | Dependent product type.
    Pi [ExtentParam v] (ArrayType f v)
  | -- | Dependent sum type.
    Sigma [ExtentParam v] (ArrayType f v)

deriving instance (Eq v) => Eq (AtomType NoInfo v)

deriving instance (Eq v) => Eq (AtomType Info v)

deriving instance (Show v) => Show (AtomType NoInfo v)

deriving instance (Show v) => Show (AtomType Info v)

instance (Show v, Pretty v, Pretty (f v)) => Pretty (AtomType f v) where
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

data ArrayType f v
  = -- | An array type literal consisting of an atom type and a shape.
    A
      { arrayTypeScalar :: AtomType f v,
        arrayTypeShape :: Shape v
      }
  | -- | An array type var. Array type vars only truly exist in the source.
    -- During typechecking, they are replaced with an atom type var and a shape
    -- var.
    ArrayTypeVar
      { arrayTypeVar :: v,
        arrayTypeVarScalar :: f v,
        arrayTypeVarShape :: f v
      }

deriving instance (Eq v) => Eq (ArrayType NoInfo v)

deriving instance (Eq v) => Eq (ArrayType Info v)

deriving instance (Show v) => Show (ArrayType NoInfo v)

deriving instance (Show v) => Show (ArrayType Info v)

instance (Show v, Pretty v, Pretty (f v)) => Pretty (ArrayType f v) where
  pretty (A t s) = parens $ "A" <+> pretty t <+> pretty s
  pretty (ArrayTypeVar v _ _) = "*" <> pretty v

-- | Types.
data Type f v
  = AtomType (AtomType f v)
  | ArrayType (ArrayType f v)

deriving instance (Show v) => Show (Type NoInfo v)

deriving instance (Show v) => Show (Type Info v)

instance (Show v, Pretty v, Pretty (f v)) => Pretty (Type f v) where
  pretty (AtomType t) = pretty t
  pretty (ArrayType t) = pretty t

-- | Make a scalar array.
mkScalarArrayType :: AtomType f v -> ArrayType f v
mkScalarArrayType = flip A mempty

-- | Get the element type.
elemType :: Type f v -> AtomType f v
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

-- | Atoms. An @Atom f v@ is an 'Atom' whose variables have type @v@ and with
-- type annotations of type @f (Type v)@. When @f = NoInfo@, the type
-- @NoInfo (Type v)@ only has a single inhabitant (namely 'NoInfo').
data Atom f v
  = -- | Base values.
    Base Base (f (AtomType f v)) SourcePos
  | -- | Term lambda.
    Lambda [(v, ArrayType f v)] (Exp f v) (f (AtomType f v)) SourcePos
  | -- | Type lambda.
    TLambda [TypeParam v] (Exp f v) (f (AtomType f v)) SourcePos
  | -- | Index lambda.
    ILambda [ExtentParam v] (Exp f v) (f (AtomType f v)) SourcePos
  | -- | Boxed expression.
    Box [Extent v] (Exp f v) (AtomType f v) SourcePos

deriving instance (Show v) => Show (Atom NoInfo v)

deriving instance (Show v) => Show (Atom Info v)

instance (Show v, Pretty v, Pretty (f (Type f v)), Pretty (f v)) => Pretty (Atom f v) where
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
  = BindVal v (Maybe (ArrayType f v)) (Exp f v) SourcePos
  | BindType (TypeParam v) (Type f v) SourcePos
  | BindExtent (ExtentParam v) (Extent v) SourcePos
  | BindFun v [(v, (ArrayType f v))] (Maybe (ArrayType f v)) (Exp f v) SourcePos
  | BindTFun v [TypeParam v] (Maybe (ArrayType f v)) (Exp f v) SourcePos
  | BindIFun v [ExtentParam v] (Maybe (ArrayType f v)) (Exp f v) SourcePos

deriving instance (Show v) => Show (Bind NoInfo v)

deriving instance (Show v) => Show (Bind Info v)

instance (Show v, Pretty v, Pretty (f (Type f v)), Pretty (f v)) => Pretty (Bind f v) where
  pretty (BindVal v t e _) =
    parens $ pretty v <+> pretty t <+> pretty e
  pretty (BindFun f params mt body _) =
    parens $
      pretty f
        <+> parens (hsep (map (\(v, ty) -> parens $ pretty v <+> pretty ty) params))
        <+> pretty mt
        <+> pretty body
  pretty (BindTFun f params mt body _) =
    parens $
      pretty f
        <+> parens (hsep (map pretty params))
        <+> pretty mt
        <+> pretty body
  pretty (BindIFun f params mt body _) =
    parens $
      pretty f
        <+> parens (hsep (map pretty params))
        <+> pretty mt
        <+> pretty body
  pretty (BindType tvar t _) =
    parens $ pretty tvar <+> pretty t
  pretty (BindExtent ivar extent _) =
    parens $ pretty ivar <+> pretty extent

-- | Expressions.
data Exp f v
  = -- | Variables.
    Var v (f (ArrayType f v)) SourcePos
  | -- | Array literals.
    Array [Int] [Atom f v] (f (ArrayType f v)) SourcePos
  | -- | Empty arrays.
    EmptyArray [Int] (AtomType f v) (f (ArrayType f v)) SourcePos
  | -- | Frame literals.
    Frame [Int] [Exp f v] (f (ArrayType f v)) SourcePos
  | -- | Empty frames.
    EmptyFrame [Int] (AtomType f v) (f (ArrayType f v)) SourcePos
  | -- | Function application.
    App (Exp f v) [Exp f v] (f (ArrayType f v, Shape v)) SourcePos
  | -- | Type application.
    TApp (Exp f v) [Type f v] (f (ArrayType f v)) SourcePos
  | -- | Index application.
    IApp (Exp f v) [Extent v] (f (ArrayType f v)) SourcePos
  | -- | Unboxing.
    Unbox [ExtentParam v] v (Exp f v) (Exp f v) (f (ArrayType f v)) SourcePos
  | -- | Let
    Let [Bind f v] (Exp f v) (f (ArrayType f v)) SourcePos

deriving instance (Show v) => Show (Exp NoInfo v)

deriving instance (Show v) => Show (Exp Info v)

instance (Show v, Pretty v, Pretty (f (Type f v)), Pretty (f v)) => Pretty (Exp f v) where
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

arrayifyType :: Type f VName -> Type f VName
arrayifyType (AtomType t) = ArrayType $ A t mempty
arrayifyType t@ArrayType {} = t

arrayTypeView :: Type f v -> ((AtomType f v, Shape v) -> a) -> a
arrayTypeView (ArrayType (A t s)) m = m (t, s)
arrayTypeView (AtomType t) m = m (t, mempty)

instance Pretty Pos where
  pretty = pretty . unPos

instance Pretty SourcePos where
  pretty (SourcePos file l col) =
    pretty file <> ":" <> pretty l <> ":" <> pretty col
