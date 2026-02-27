module Uniquify (uniquify) where

import Control.Monad.RWS
import Data.Map qualified as M
import Data.Text (Text)
import Intrinsics
import Syntax
import Uniquify.Monad
import Uniquify.Type
import VName

uniquify :: Exp NoInfo Text -> Exp NoInfo VName
uniquify = fst . runUniquify initEnv maxIntrinsicTag . uniquifyExp
  where
    initEnv =
      mempty
        { vnameEnvVars =
            M.fromList $ map (\(k, _) -> (varName k, k)) $ M.toList intrinsics
        }

-- | Type uniquify an ununiquifyed 'Exp'.
uniquifyExp :: (MonadUniquify m) => Exp NoInfo Text -> m (Exp NoInfo VName)
uniquifyExp (Var v _ pos) = do
  vname <- fetchVar v
  pure $ Var vname NoInfo pos
uniquifyExp (Array ns as _ pos) = do
  as' <- mapM uniquifyAtom as
  pure $ Array ns as' NoInfo pos
uniquifyExp (EmptyArray ns t _ pos) = do
  t' <- uniquifyTypeExp t
  pure $ EmptyArray ns t' NoInfo pos
uniquifyExp (Frame ns es _ pos) = do
  es' <- mapM uniquifyExp es
  pure $ Frame ns es' NoInfo pos
uniquifyExp (EmptyFrame ns t _ pos) = do
  t' <- uniquifyTypeExp t
  pure $ EmptyFrame ns t' NoInfo pos
uniquifyExp (App f args _ pos) = do
  f' <- uniquifyExp f
  args' <- mapM uniquifyExp args
  pure $ App f' args' NoInfo pos
uniquifyExp (TApp f ts _ pos) = do
  f' <- uniquifyExp f
  ts' <- mapM uniquifyTypeExp ts
  pure $ TApp f' ts' NoInfo pos
uniquifyExp (IApp f is _ pos) = do
  f' <- uniquifyExp f
  is' <- mapM (mapExtent (fmap Dim . uniquifyDim) (fmap Shape . uniquifyShape)) is
  pure $ IApp f' is' NoInfo pos
uniquifyExp (Unbox is x_e box body _ pos) = do
  binds withExtentParam is $ \is' -> do
    box' <- uniquifyExp box
    withParam x_e $ \x_e' -> do
      body' <- uniquifyExp body
      pure $ Unbox is' x_e' box' body' NoInfo pos
uniquifyExp (Let bs e _ pos) = do
  binds withBind bs $ \bs' -> do
    e' <- uniquifyExp e
    pure $ Let bs' e' NoInfo pos

uniquifyMaybeTypeExp ::
  (MonadUniquify m) =>
  Maybe (TypeExp Text) -> m (Maybe (TypeExp VName))
uniquifyMaybeTypeExp Nothing = pure Nothing
uniquifyMaybeTypeExp (Just t) = Just <$> uniquifyTypeExp t

withBind :: (MonadUniquify m) => Bind NoInfo Text -> (Bind NoInfo VName -> m a) -> m a
withBind (BindVal v mt ve pos) m = do
  ve' <- uniquifyExp ve
  mt' <- uniquifyMaybeTypeExp mt
  withParam v $ \vname ->
    m $ BindVal vname mt' ve' pos
withBind (BindFun f params mt body _ pos) m = do
  mt' <- uniquifyMaybeTypeExp mt
  (params', body') <-
    binds (withPatParam uniquifyTypeExp) params $ \params' -> do
      body' <- uniquifyExp body
      pure (params', body')
  withParam f $ \f' ->
    m $ BindFun f' params' mt' body' NoInfo pos
withBind (BindTFun f params mt body _ pos) m =
  binds withTypeParam params $ \params' -> do
    body' <- uniquifyExp body
    mt' <- uniquifyMaybeTypeExp mt
    withParam f $ \f' ->
      m $ BindTFun f' params' mt' body' NoInfo pos
withBind (BindIFun f params mt body _ pos) m =
  binds withExtentParam params $ \params' -> do
    body' <- uniquifyExp body
    mt' <- uniquifyMaybeTypeExp mt
    withParam f $ \f' ->
      m $ BindIFun f' params' mt' body' NoInfo pos
withBind (BindType tvar t _ pos) m =
  withType uniquifyTypeExp (tvar, t) $ \(tvar', t') ->
    m $ BindType tvar' t' NoInfo pos
withBind (BindExtent ivar extent pos) m =
  withExtent uniquifyExtent pos (ivar, extent) $ \(ivar', extent') ->
    m $ BindExtent ivar' extent' pos

-- | Type uniquify an ununiquifyed 'Atom'.
uniquifyAtom :: (MonadUniquify m) => Atom NoInfo Text -> m (Atom NoInfo VName)
uniquifyAtom (Base b _ pos) =
  pure $ Base b NoInfo pos
uniquifyAtom (Lambda params e _ pos) = do
  binds (withPatParam uniquifyTypeExp) params $ \params' -> do
    e' <- uniquifyExp e
    pure $ Lambda params' e' NoInfo pos
uniquifyAtom (TLambda ps e _ pos) =
  binds withTypeParam ps $ \ps' -> do
    e' <- uniquifyExp e
    pure $ TLambda ps' e' NoInfo pos
uniquifyAtom (ILambda ps e _ pos) =
  binds withExtentParam ps $ \ps' -> do
    e' <- uniquifyExp e
    pure $ ILambda ps' e' NoInfo pos
uniquifyAtom (Box extent e box_t _ pos) = do
  extent' <- mapM uniquifyExtent extent
  e' <- uniquifyExp e
  box_t' <- uniquifyTypeExp box_t
  pure $ Box extent' e' box_t' NoInfo pos

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
uniquifyTypeExp (TEArrow ts r pos) =
  TEArrow <$> mapM uniquifyTypeExp ts <*> uniquifyTypeExp r <*> pure pos
uniquifyTypeExp (TEForall params t pos) =
  binds withTypeParam params $ \params' ->
    TEForall params' <$> uniquifyTypeExp t <*> pure pos
uniquifyTypeExp (TEPi params t pos) =
  binds withExtentParam params $ \params' ->
    TEPi params' <$> uniquifyTypeExp t <*> pure pos
uniquifyTypeExp (TESigma params t pos) = do
  binds withExtentParam params $ \params' -> do
    TESigma params' <$> uniquifyTypeExp t <*> pure pos
