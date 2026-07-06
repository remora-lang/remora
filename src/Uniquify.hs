module Uniquify (uniquify, uniquifyExp) where

import Control.Monad.State (state)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Semigroup (sconcat)
import Data.Text (Text)
import Intrinsics
import Pass
import Syntax
import Uniquify.Monad
import Uniquify.Type
import VName

uniquify :: UncheckedProg -> PassM UniqueProg
uniquify p =
  state $ \tag -> runUniquify initEnv tag $ uniquifyProg p

uniquifyExp :: UncheckedExp -> PassM UniqueExp
uniquifyExp e =
  state $ \tag -> runUniquify initEnv tag $ uniquifyExp' e

initEnv :: Env
initEnv =
  mempty
    { vnameEnvVars =
        M.fromList $
          map (\(k, _) -> (varName k, k)) $
            M.toList intrinsics
    }

uniquifyProg :: (MonadUniquify m) => UncheckedProg -> m UniqueProg
uniquifyProg (Prog decs) = Prog <$> mapM uniquifyDecl decs

uniquifyDecl :: (MonadUniquify m) => UncheckedDecl -> m UniqueDecl
uniquifyDecl (Def b) = withBind b $ pure . Def . NE.head
uniquifyDecl (Entry f params mt body _ pos) = do
  mt' <- uniquifyMaybeTypeExp mt
  (params', body') <-
    binds (withPatParam uniquifyTypeExp) params $ \params' -> do
      body' <- uniquifyExp' body
      pure (params', body')
  withParam f $ \f' ->
    pure $ Entry f' params' mt' body' NoInfo pos

uniquifyExp' :: (MonadUniquify m) => UncheckedExp -> m UniqueExp
uniquifyExp' (Var v _ pos) = do
  vname <- fetchVar v
  pure $ Var vname NoInfo pos
uniquifyExp' (Array ns as _ pos) = do
  as' <- mapM uniquifyAtom as
  pure $ Array ns as' NoInfo pos
uniquifyExp' (EmptyArray ns t _ pos) = do
  t' <- uniquifyTypeExp t
  pure $ EmptyArray ns t' NoInfo pos
uniquifyExp' (Frame ns es _ pos) = do
  es' <- mapM uniquifyExp' es
  pure $ Frame ns es' NoInfo pos
uniquifyExp' (EmptyFrame ns t _ pos) = do
  t' <- uniquifyTypeExp t
  pure $ EmptyFrame ns t' NoInfo pos
uniquifyExp' (App f arg _ pos) = do
  f' <- uniquifyExp' f
  arg' <- uniquifyExp' arg
  pure $ App f' arg' NoInfo pos
uniquifyExp' (TApp f t _ pos) = do
  f' <- uniquifyExp' f
  t' <- uniquifyTypeExp t
  pure $ TApp f' t' NoInfo pos
uniquifyExp' (IApp f i _ pos) = do
  f' <- uniquifyExp' f
  i' <- mapISpace (fmap Dim . uniquifyDim) (fmap Shape . uniquifyShape) i
  pure $ IApp f' i' NoInfo pos
uniquifyExp' (Unbox ep x_e box body _ pos) = do
  withISpaceParam ep $ \ep' -> do
    box' <- uniquifyExp' box
    withParam x_e $ \x_e' -> do
      body' <- uniquifyExp' body
      pure $ Unbox ep' x_e' box' body' NoInfo pos
uniquifyExp' (Let bs e _ pos) = do
  bindsNE withBind bs $ \bss -> do
    e' <- uniquifyExp' e
    pure $ Let (sconcat bss) e' NoInfo pos

uniquifyMaybeTypeExp ::
  (MonadUniquify m) =>
  Maybe (TypeExp Text) -> m (Maybe (TypeExp VName))
uniquifyMaybeTypeExp Nothing = pure Nothing
uniquifyMaybeTypeExp (Just t) = Just <$> uniquifyTypeExp t

withBind :: (MonadUniquify m) => UncheckedBind -> (NonEmpty UniqueBind -> m a) -> m a
withBind (BindVal v mt ve pos) m = do
  ve' <- uniquifyExp' ve
  mt' <- uniquifyMaybeTypeExp mt
  withParam v $ \vname ->
    m $ pure $ BindVal vname mt' ve' pos
withBind (BindFun f params mt body _ pos) m = do
  mt' <- uniquifyMaybeTypeExp mt
  (params', body') <-
    binds (withPatParam uniquifyTypeExp) params $ \params' -> do
      body' <- uniquifyExp' body
      pure (params', body')
  withParam f $ \f' ->
    m $ pure $ BindFun f' params' mt' body' NoInfo pos
withBind (BindTFun f params mt body _ pos) m =
  withSourceTypeParams params $ \atoms shapes -> do
    body' <- uniquifyExp' body
    mt' <- uniquifyMaybeTypeExp mt
    let body'' = foldr (\s b -> mkScalar (ILambda (ShapeParam s) b NoInfo pos)) body' shapes
    withParam f $ \f' ->
      m $ pure $ BindTFun f' atoms mt' body'' NoInfo pos
withBind (BindIFun f params mt body _ pos) m =
  binds withISpaceParam params $ \params' -> do
    body' <- uniquifyExp' body
    mt' <- uniquifyMaybeTypeExp mt
    withParam f $ \f' ->
      m $ pure $ BindIFun f' params' mt' body' NoInfo pos
withBind (BindType (TEArrayTypeParam v) t _ pos) m = do
  t' <- uniquifyTypeExp t
  case t' of
    TEArray atomTE shapeSh _ ->
      withArrayTypeParam v $ \et s ->
        m $
          NE.fromList $
            [ BindType (AtomTypeParam et) atomTE NoInfo pos,
              BindISpace (ShapeParam s) (Shape shapeSh) pos
            ]
    _ ->
      error "Uniquify: array type alias must be bound to an array type"
withBind (BindType tvar t _ pos) m =
  withType uniquifyTypeExp (tvar, t) $ \(tvar', t') ->
    m $ pure $ BindType tvar' t' NoInfo pos
withBind (BindISpace ivar ispace pos) m =
  withISpace uniquifyISpace pos (ivar, ispace) $ \(ivar', ispace') ->
    m $ pure $ BindISpace ivar' ispace' pos

uniquifyAtom :: (MonadUniquify m) => UncheckedAtom -> m UniqueAtom
uniquifyAtom (Base b _ pos) =
  pure $ Base b NoInfo pos
uniquifyAtom (Lambda param e _ pos) =
  withPatParam uniquifyTypeExp param $ \param' -> do
    e' <- uniquifyExp' e
    pure $ Lambda param' e' NoInfo pos
uniquifyAtom (TLambda (TEArrayTypeParam v) e _ pos) =
  withArrayTypeParam v $ \et s -> do
    e' <- uniquifyExp' e
    pure $
      TLambda
        (AtomTypeParam et)
        (mkScalar (ILambda (ShapeParam s) e' NoInfo pos))
        NoInfo
        pos
uniquifyAtom (TLambda p e _ pos) =
  withSourceTypeParam p $ \p' -> do
    e' <- uniquifyExp' e
    pure $ TLambda p' e' NoInfo pos
uniquifyAtom (ILambda p e _ pos) =
  withISpaceParam p $ \p' -> do
    e' <- uniquifyExp' e
    pure $ ILambda p' e' NoInfo pos
uniquifyAtom (Box ispace e box_t _ pos) = do
  ispace' <- uniquifyISpace ispace
  e' <- uniquifyExp' e
  box_t' <- uniquifyTypeExp box_t
  pure $ Box ispace' e' box_t' NoInfo pos

uniquifyTypeExp :: (MonadUniquify m) => TypeExp Text -> m (TypeExp VName)
uniquifyTypeExp (TEAtomVar v pos) = do
  vname <- fetchAtomTypeVar v
  pure $ TEAtomVar vname pos
uniquifyTypeExp (TEArrayVar v pos) = do
  ArrayTypeVarBundle _ et_vname s_vname <- fetchArrayTypeVar v
  pure $ TEArray (TEAtomVar et_vname pos) (ShapeVar s_vname) pos
uniquifyTypeExp (TEBool pos) = pure $ TEBool pos
uniquifyTypeExp (TEInt pos) = pure $ TEInt pos
uniquifyTypeExp (TEFloat pos) = pure $ TEFloat pos
uniquifyTypeExp (TEArray t s pos) =
  TEArray <$> uniquifyTypeExp t <*> uniquifyShape s <*> pure pos
uniquifyTypeExp (TEArrow t1 t2 pos) =
  TEArrow <$> uniquifyTypeExp t1 <*> uniquifyTypeExp t2 <*> pure pos
uniquifyTypeExp (TEForall params t pos) =
  bindsNE withTypeParamExp params $ \params' ->
    TEForall params' <$> uniquifyTypeExp t <*> pure pos
uniquifyTypeExp (TEPi params t pos) =
  bindsNE withISpaceParam params $ \params' ->
    TEPi params' <$> uniquifyTypeExp t <*> pure pos
uniquifyTypeExp (TESigma params t pos) =
  bindsNE withISpaceParam params $ \params' ->
    TESigma params' <$> uniquifyTypeExp t <*> pure pos
