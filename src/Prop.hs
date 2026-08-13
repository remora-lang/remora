{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Prop
  ( scalarTypeOf,
    baseTypeOf,
    HasArrayType (..),
    HasType (..),
    HasShape (..),
    isScalar,
    isLambda,
    IsType (..),
    (@=),
    (\\),
    (.<=.),
    maximumShape,
    convertTypeExp,
    convertAtomTypeExp,
    convertArrayTypeExp,
    unfoldArrow,
    isFunctionType,
    peelArrayType,
    arrayOf,
    findRet,
    scalarize,
    frameOf,
    mkFunBind,
    mkApp,
    mkVar,
    mkParam,
    patVarTypes,
    applyTypeArg,
    applyISpaceArg,
    mkTypeApp,
    mkISpaceApp,
    bindNameOf,
    addTypeParams,
    addISpaceParams,
    addTermParams,
    unboxType,
    insertBinds,
    insertBindsExp,
  )
where

import Binds
import Control.Applicative
import Control.Monad.State (MonadState)
import Data.Bifunctor (first, second)
import Data.Foldable (find)
import Data.Functor qualified
import Data.List.NonEmpty qualified as NE
import Data.Maybe
import ISpace
import Prettyprinter
import Substitute
import Symbolic qualified
import Syntax
import Util
import VName

class HasArrayType x where
  arrayTypeOf :: x -> ArrayType VName
  arrayTypeOf = normType . arrayTypeOf_

  arrayTypeOf_ :: x -> ArrayType VName

instance HasArrayType Exp where
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
  arrayTypeOf_ (Struct _ (Info (t, _)) _) = t
  arrayTypeOf_ (FieldProj _ _ (Info t) _) = t

instance HasArrayType Pat where
  arrayTypeOf_ (PatId _ _ (Info t) _) = t

instance HasArrayType (Type VName) where
  arrayTypeOf_ (AtomType et) = et :@ mempty
  arrayTypeOf_ (ArrayType t) = t

instance HasArrayType Bind where
  arrayTypeOf_ (BindVal _ _ e _) = arrayTypeOf e
  arrayTypeOf_ (BindFun _ _ _ _ (Info t) _) = mkScalarArrayType t
  arrayTypeOf_ (BindTFun _ _ _ _ (Info t) _) = mkScalarArrayType t
  arrayTypeOf_ (BindIFun _ _ _ _ (Info t) _) = mkScalarArrayType t
  arrayTypeOf_ b = error $ "arrayTypeOf: not a value bind: " <> prettyString b

scalarTypeOf :: Atom -> AtomType VName
scalarTypeOf = normType . scalarTypeOf_
  where
    scalarTypeOf_ (Base _ (Info t) _) = t
    scalarTypeOf_ (Lambda _ _ (Info t) _) = t
    scalarTypeOf_ (TLambda _ _ (Info t) _) = t
    scalarTypeOf_ (ILambda _ _ (Info t) _) = t
    scalarTypeOf_ (Box _ _ _ (Info t) _) = t

baseTypeOf :: Base -> AtomType VName
baseTypeOf BoolVal {} = Bool
baseTypeOf IntVal {} = Int
baseTypeOf FloatVal {} = Float

isLambda :: Atom -> Bool
isLambda Lambda {} = True
isLambda TLambda {} = True
isLambda ILambda {} = True
isLambda _ = False

scalarize :: Atom -> Exp
scalarize a =
  Array mempty (pure a) (Info $ mkScalarArrayType $ scalarTypeOf a) (posOf a)

mkFunBind :: VName -> NE.NonEmpty Pat -> Exp -> Bind
mkFunBind name params body =
  BindFun
    name
    params
    Nothing
    body
    (Info $ (arrayTypeOf <$> params) `arrowType` arrayTypeOf body)
    noSrcPos

mkApp :: Exp -> [Exp] -> Exp
mkApp = foldl app
  where
    app f arg =
      case arrayTypeOf f of
        (_ :-> ra) :@ _ -> App f arg (Info (ra, mempty)) (posOf f)
        _ -> error "mkApp: illegal non-func"

mkVar :: VName -> ArrayType VName -> Exp
mkVar v t = Var v (Info t) noSrcPos

mkParam :: VName -> ArrayType VName -> Pat
mkParam v t = PatId v (ArrayType t) (Info t) noSrcPos

patVarTypes :: [Pat] -> [(VName, ArrayType VName)]
patVarTypes = map $ \p -> (patVar p, arrayTypeOf p)

convertISpaceParam :: ISpaceParam VName -> ISpace VName
convertISpaceParam (ShapeParam v) = Shape $ ShapeVar v
convertISpaceParam (DimParam v) = Dim $ DimVar v

applyTypeArg :: Exp -> AtomType VName -> Exp
applyTypeArg e at =
  case arrayTypeOf e of
    Forall p r :@ frame ->
      TApp e (AtomType at) (Info $ instantiated r frame) noSrcPos
      where
        instantiated t =
          arrayOf $ ArrayType $ substitute (substAtomVar (unTypeParam p) at) t
    _ -> error $ "applyTypeArg: not a forall: " <> prettyString e

applyISpaceArg :: Exp -> ISpace VName -> Exp
applyISpaceArg e isp =
  case arrayTypeOf e of
    Pi p r :@ frame ->
      IApp e isp (Info $ instantiated r frame) noSrcPos
      where
        instantiated t =
          arrayOf $ ArrayType $ substitute (substISpaceVar (unISpaceParam p) isp) t
    _ -> error $ "applyISpaceArg: not a pi: " <> prettyString e

mkTypeApp :: Exp -> VName -> Exp
mkTypeApp e = applyTypeArg e . AtomTypeVar

mkISpaceApp :: Exp -> ISpaceParam VName -> Exp
mkISpaceApp e = applyISpaceArg e . convertISpaceParam

bindNameOf :: Bind -> VName
bindNameOf b =
  fromMaybe (error $ "bindNameOf: nameless bind: " <> prettyString b) $ bindName b

bindToExp :: Bind -> Exp
bindToExp b =
  Let (b NE.:| []) (mkVar (bindNameOf b) t) (Info t) noSrcPos
  where
    t = arrayTypeOf b

addTypeParams :: [VName] -> Bind -> Bind
addTypeParams [] inner = inner
addTypeParams (tv : tvs) inner =
  BindTFun
    (bindNameOf inner)
    ps
    Nothing
    (bindToExp inner)
    (Info $ forallType ps $ arrayTypeOf inner)
    noSrcPos
  where
    ps = AtomTypeParam <$> tv NE.:| tvs

addISpaceParams :: [ISpaceParam VName] -> Bind -> Bind
addISpaceParams [] inner = inner
addISpaceParams (ip : ips) inner =
  BindIFun
    (bindNameOf inner)
    ps
    Nothing
    (bindToExp inner)
    (Info $ piType ps $ arrayTypeOf inner)
    noSrcPos
  where
    ps = ip NE.:| ips

addTermParams :: [Pat] -> Bind -> Bind
addTermParams [] inner = inner
addTermParams (p : ps) inner =
  case inner of
    BindFun _ ps' _ body _ _ -> mkFunBind v (NE.prependList (p : ps) ps') body
    _ -> mkFunBind v (p NE.:| ps) $ bindToExp inner
  where
    v = bindNameOf inner

frameOf :: SourcePos -> [Int] -> NE.NonEmpty Exp -> Exp
frameOf pos s es =
  flattenExp $
    Frame s es (Info $ arrayOf (typeOf $ NE.head es) (intsToShape s)) pos

insertBinds :: (MonadState s m, HasBinds s) => Prog -> m Prog
insertBinds p = do
  bs <- drainBinds
  pure $ Prog $ map Def bs <> progDecs p

insertBindsExp :: (MonadState s m, HasBinds s) => Exp -> m Exp
insertBindsExp e = do
  bs <- drainBinds
  pure $ case NE.nonEmpty bs of
    Nothing -> e
    Just bs' -> Let bs' e (Info $ arrayTypeOf e) (posOf e)

unboxType :: ISpaceParam VName -> ArrayType VName -> Maybe (ArrayType VName)
unboxType ip boxt =
  case arrayTypeAtom boxt of
    Sigma ps t -> Just $ substitute (renameVar (unISpaceParam ps) (unISpaceParam ip)) t
    _ -> Nothing

-- | Things that have a type.
class HasType x where
  -- | Returns a normalized type.
  typeOf :: x -> Type VName
  typeOf = normType . typeOf_

  typeOf_ :: x -> Type VName

instance HasType Base where
  typeOf_ = AtomType . baseTypeOf

instance HasType Atom where
  typeOf_ = AtomType . scalarTypeOf

instance HasType Exp where
  typeOf_ = ArrayType . arrayTypeOf

instance HasType Pat where
  typeOf_ = ArrayType . arrayTypeOf

-- | Things that have a shape.
class HasShape x where
  shapeOf :: x -> Shape VName
  shapeOf = normShape . shapeOf_

  shapeOf_ :: x -> Shape VName

instance HasShape (AtomBase te tp f VName) where
  shapeOf_ _ = mempty

instance HasShape (ArrayType VName) where
  shapeOf_ (_ :@ s) = s

instance HasShape (Type VName) where
  shapeOf_ (ArrayType t) = shapeOf t
  shapeOf_ _ = mempty

instance HasShape Exp where
  shapeOf_ = shapeOf_ . typeOf_

instance HasShape (Shape VName) where
  shapeOf_ = id

-- | Things that are types.
class IsType x where
  normType :: x -> x
  (~=) :: x -> x -> Bool
  atomType :: x -> AtomType VName

infix 4 ~=

instance IsType (AtomType VName) where
  normType (t :-> r) = normType t :-> normType r
  normType (Forall pts t) = Forall pts $ normType t
  normType (Pi pts t) = Pi pts $ normType t
  normType (Sigma pts t) = Sigma pts $ normType t
  normType (Record fs) = Record $ fmap (second normType) fs
  normType t = t

  (p :-> r) ~= (q :-> t) = p ~= q && r ~= t
  Forall p r ~= Forall q t =
    substitute (renameVar (unTypeParam p) (unTypeParam q)) r ~= t
  Pi p r ~= Pi q t =
    substitute (renameVar (unISpaceParam p) (unISpaceParam q)) r ~= t
  Sigma p r ~= Sigma q t =
    substitute (renameVar (unISpaceParam p) (unISpaceParam q)) r ~= t
  Record f1s ~= Record f2s = all cmpFields allFields
    where
      allFields = NE.nub $ NE.append (fmap fst f1s) (fmap fst f2s)
      cmpFields fName = case (findField fName f1s, findField fName f2s) of
        (Just t1, Just t2) -> t1 ~= t2
        _ -> False
      findField fn fs = fmap snd (find ((== fn) . fst) fs)
  t ~= r = t == r

  atomType = id

instance IsType (ArrayType VName) where
  normType (t :@ s) = normType t :@ normShape s

  (t :@ s) ~= (y :@ x) =
    (t ~= y) && (s Symbolic.@= x)

  atomType = arrayTypeAtom

instance IsType (Type VName) where
  normType (AtomType t) = AtomType $ normType t
  normType (ArrayType t) = ArrayType $ normType t

  AtomType t ~= AtomType r = t ~= r
  ArrayType t ~= ArrayType r = t ~= r
  _ ~= _ = False

  atomType (AtomType t) = atomType t
  atomType (ArrayType t) = atomType t

-- | Naive shape equality.
(@=) :: (Ord v) => Shape v -> Shape v -> Bool
s @= t = normShape s == normShape t

isScalar :: (HasShape x) => x -> Bool
isScalar x = shapeOf x @= mempty

infix 4 @=

-- | Unfold a curried function type into its parameter types and return type.
unfoldArrow :: (Ord v) => ArrayType v -> ([ArrayType v], ArrayType v)
unfoldArrow ((param :-> ret) :@ s)
  | s @= mempty = first (param :) (unfoldArrow ret)
unfoldArrow t = ([], t)

isFunctionType :: ArrayType v -> Bool
isFunctionType ((_ :-> _) :@ _) = True
isFunctionType _ = False

peelArrayType :: (Ord v) => ArrayType v -> ArrayType v
peelArrayType (a :@ s) = a :@ peelShape s

arrayOf :: Type v -> Shape v -> ArrayType v
arrayOf (AtomType et) s = et :@ s
arrayOf (ArrayType (et :@ s')) s = et :@ (s <> s')

findRet :: (Ord v) => AtomType v -> ArrayType v
findRet = snd . unfoldArrow . mkScalarArrayType

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
s \\ t = error $ unlines [show s, show t]

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
    . foldMap ((: []) . normShape)

convertTypeExp :: (Ord v) => TypeExp v -> Maybe (Type v)
convertTypeExp t =
  (AtomType <$> convertAtomTypeExp t)
    <|> (ArrayType <$> convertArrayTypeExp t)

convertTypeParamExp :: TypeParamExp v -> Maybe (TypeParam v)
convertTypeParamExp (TEAtomTypeParam v) = Just (AtomTypeParam v)
convertTypeParamExp (TEArrayTypeParam _) = Nothing

convertAtomTypeExp :: (Ord v) => TypeExp v -> Maybe (AtomType v)
convertAtomTypeExp (TEAtomVar v _) = pure $ AtomTypeVar v
convertAtomTypeExp (TEBool _) = pure Bool
convertAtomTypeExp (TEInt _) = pure Int
convertAtomTypeExp (TEFloat _) = pure Float
convertAtomTypeExp (TEArrow t1 t2 _) =
  (:->) <$> convertArrayTypeExp t1 <*> convertArrayTypeExp t2
convertAtomTypeExp (TEForall ps t _) = do
  params <- traverse convertTypeParamExp ps
  t' <- convertArrayTypeExp t
  pure $ foldr (\p r -> Forall p (mkScalarArrayType r)) (Forall (NE.last params) t') (NE.init params)
convertAtomTypeExp (TEPi ps t _) = do
  t' <- convertArrayTypeExp t
  pure $ foldr (\p r -> Pi p (mkScalarArrayType r)) (Pi (NE.last ps) t') (NE.init ps)
convertAtomTypeExp (TESigma ps t _) = do
  t' <- convertArrayTypeExp t
  pure $ foldr (\p r -> Sigma p (mkScalarArrayType r)) (Sigma (NE.last ps) t') (NE.init ps)
convertAtomTypeExp (TERecord fs _) = do
  let (fNames, ts) = Data.Functor.unzip fs
  ts' <- mapM convertArrayTypeExp ts
  pure $ Record (NE.zip fNames ts')
convertAtomTypeExp _ = Nothing

convertArrayTypeExp :: (Ord v) => TypeExp v -> Maybe (ArrayType v)
convertArrayTypeExp (TEArray t s _) = do
  t' <- convertArrayTypeExp t
  pure $ arrayOf (ArrayType t') s
convertArrayTypeExp (TEArrayVar v _) =
  pure $ AtomTypeVar v :@ ShapeVar v
convertArrayTypeExp t =
  (:@) <$> convertAtomTypeExp t <*> pure mempty
