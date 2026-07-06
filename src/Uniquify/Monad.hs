module Uniquify.Monad where

import Control.Monad.RWS
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Prettyprinter
import Syntax
import Util
import VName

data ArrayTypeVarBundle v
  = ArrayTypeVarBundle
  { arrayTypeParam :: v,
    atomTypeParam :: v,
    shapeVar :: v
  }
  deriving (Eq, Show)

-- | Environment that maps the various categories of source names to 'VName's.
data Env
  = Env
  { -- | 'Text' vars to 'VName's.
    vnameEnvVars :: Map Text VName,
    -- | 'Text' atom type vars to 'VName's.
    vnameEnvAtomTypeVars :: Map Text VName,
    -- | 'Text' array type vars to 'VName's.
    vnameEnvArrayTypeVarBundles :: Map Text (ArrayTypeVarBundle VName),
    -- | 'Text' dim vars to 'VName's.
    vnameEnvDimVars :: Map Text VName,
    -- | 'Text' shape vars to 'VNames.
    vnameEnvShapeVars :: Map Text VName
  }
  deriving (Eq, Show)

instance Semigroup Env where
  Env vs1 ats1 ts1 ds1 ss1 <> Env vs2 ats2 ts2 ds2 ss2 =
    Env (vs1 <> vs2) (ats1 <> ats2) (ts1 <> ts2) (ds1 <> ds2) (ss1 <> ss2)

instance Monoid Env where
  mempty = Env mempty mempty mempty mempty mempty

type MonadUniquify m =
  ( Monad m,
    MonadReader Env m,
    MonadState Tag m,
    MonadVName m
  )

newtype UniquifyM a = UniquifyM {runUniquifyM :: RWS Env () Tag a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader Env,
      MonadState Tag
    )

instance MonadVName UniquifyM where
  getVarTag = get
  putVarTag = put

runUniquify :: Env -> Tag -> UniquifyM a -> (a, Tag)
runUniquify env tag m =
  let (a, tag', _) = runRWS (runUniquifyM m) env tag
   in (a, tag')

-- | Lookup something in the environment.
lookupEnv ::
  ( MonadReader env m,
    Pretty a
  ) =>
  Text ->
  a ->
  (env -> Maybe b) ->
  m b
lookupEnv t a look = do
  mb <- asks look
  case mb of
    Nothing -> error $ "Unknown " <> T.unpack t <> " var: " <> prettyString a
    Just b -> pure b

-- | Fetch the unique name for a variable.
fetchVar :: (MonadUniquify m) => Text -> m VName
fetchVar v =
  lookupEnv "text" v $ (M.!? v) . vnameEnvVars

-- | Fetch the unique name for an atom type variable.
fetchAtomTypeVar :: (MonadUniquify m) => Text -> m VName
fetchAtomTypeVar v =
  lookupEnv "text atom type" v $ (M.!? v) . vnameEnvAtomTypeVars

-- | Fetch the unique name for an array type variable.
fetchArrayTypeVar :: (MonadUniquify m) => Text -> m (ArrayTypeVarBundle VName)
fetchArrayTypeVar v =
  lookupEnv "text array type" v $ (M.!? v) . vnameEnvArrayTypeVarBundles

-- | Fetch the unique name for a dim variable.
fetchDimVar :: (MonadUniquify m) => Text -> m VName
fetchDimVar v =
  lookupEnv "text dim" v $ (M.!? v) . vnameEnvDimVars

-- | Fetch the unique name for a shape variable.
fetchShapeVar :: (MonadUniquify m) => Text -> m VName
fetchShapeVar v =
  lookupEnv "text shape" v $ (M.!? v) . vnameEnvShapeVars

withPatParam ::
  (MonadUniquify m) =>
  (TypeExp Text -> m (TypeExp VName)) ->
  UncheckedPat ->
  (UniquePat -> m a) ->
  m a
withPatParam uniquifyType (PatId v t _ pos) m = do
  vname <- newVName v
  t' <- uniquifyType t
  local (\env -> env {vnameEnvVars = M.insert v vname $ vnameEnvVars env}) $
    m $
      PatId vname t' NoInfo pos

-- | Bind a source parameter into a local environment.
withParam ::
  (MonadUniquify m) =>
  Text ->
  (VName -> m a) ->
  m a
withParam v m = do
  vname <- newVName v
  local (\env -> env {vnameEnvVars = M.insert v vname $ vnameEnvVars env}) $
    m vname

-- | Bind a source type parameter into a local environment.
withTypeParam :: (MonadUniquify m) => TypeParam Text -> (TypeParam VName -> m a) -> m a
withTypeParam (AtomTypeParam v) m = do
  vname <- newVName v
  local
    ( \env ->
        env
          { vnameEnvAtomTypeVars =
              M.insert v vname $ vnameEnvAtomTypeVars env
          }
    )
    $ m (AtomTypeParam vname)

-- | Bind a source-level term type parameter.
withSourceTypeParam :: (MonadUniquify m) => TypeParamExp Text -> (TypeParam VName -> m a) -> m a
withSourceTypeParam (TEAtomTypeParam v) m = do
  vname <- newVName v
  local
    ( \env ->
        env
          { vnameEnvAtomTypeVars =
              M.insert v vname $ vnameEnvAtomTypeVars env
          }
    )
    $ m (AtomTypeParam vname)
withSourceTypeParam (TEArrayTypeParam _) _ =
  error "Uniquify.withSourceTypeParam: array type parameter binders not yet desugared"

-- | Bind a list of source term type parameters.
withSourceTypeParams :: (MonadUniquify m) => [TypeParamExp Text] -> ([TypeParam VName] -> [VName] -> m a) -> m a
withSourceTypeParams [] k = k [] []
withSourceTypeParams (TEArrayTypeParam v : ps) k =
  withArrayTypeParam v $ \et s ->
    withSourceTypeParams ps $ \atoms shapes -> k (AtomTypeParam et : atoms) (s : shapes)
withSourceTypeParams (p : ps) k =
  withSourceTypeParam p $ \p' ->
    withSourceTypeParams ps $ \atoms shapes -> k (p' : atoms) shapes

-- | Bind a source array type parameter.
withArrayTypeParam :: (MonadUniquify m) => Text -> (VName -> VName -> m a) -> m a
withArrayTypeParam v m = do
  vname <- newVName v
  et_vname <- newVName $ "*" <> v
  s_vname <- newVName $ "@" <> v
  local
    ( \env ->
        env
          { vnameEnvArrayTypeVarBundles =
              M.insert v (ArrayTypeVarBundle vname et_vname s_vname) $
                vnameEnvArrayTypeVarBundles env
          }
    )
    $ m et_vname s_vname

-- | Bind a source type parameter expression into a local environment.
withTypeParamExp :: (MonadUniquify m) => TypeParamExp Text -> (TypeParamExp VName -> m a) -> m a
withTypeParamExp (TEAtomTypeParam v) m = do
  vname <- newVName v
  local
    ( \env ->
        env
          { vnameEnvAtomTypeVars =
              M.insert v vname $ vnameEnvAtomTypeVars env
          }
    )
    $ m (TEAtomTypeParam vname)
withTypeParamExp (TEArrayTypeParam v) m = do
  vname <- newVName v
  et_vname <- newVName $ "*" <> v
  s_vname <- newVName $ "@" <> v
  local
    ( \env ->
        env
          { vnameEnvArrayTypeVarBundles =
              M.insert v (ArrayTypeVarBundle vname et_vname s_vname) $
                vnameEnvArrayTypeVarBundles env
          }
    )
    $ m (TEArrayTypeParam vname)

-- | Bind a source type parameter into a local environment.
withISpaceParam :: (MonadUniquify m) => ISpaceParam Text -> (ISpaceParam VName -> m a) -> m a
withISpaceParam p m = do
  vname <- newVName $ unISpaceParam p
  local
    ( \env ->
        case p of
          DimParam v ->
            env
              { vnameEnvDimVars =
                  M.insert v vname $ vnameEnvDimVars env
              }
          ShapeParam v ->
            env
              { vnameEnvShapeVars =
                  M.insert v vname $ vnameEnvShapeVars env
              }
    )
    $ m (vname <$ p)

-- | Bind a type binding.
withType ::
  (MonadUniquify m) =>
  (TypeExp Text -> m (TypeExp VName)) ->
  (TypeParamExp Text, TypeExp Text) ->
  ((TypeParam VName, TypeExp VName) -> m a) ->
  m a
withType uniquifyTypeExp (p, t) m =
  withSourceTypeParam p $ \p' -> do
    t' <- uniquifyTypeExp t
    m (p', t')

-- | Bind an index binding.
withISpace ::
  (MonadUniquify m) =>
  (ISpace Text -> m (ISpace VName)) ->
  SourcePos ->
  (ISpaceParam Text, ISpace Text) ->
  ((ISpaceParam VName, ISpace VName) -> m a) ->
  m a
withISpace uniquifyISpace _ (p, ext) m =
  withISpaceParam p $ \p' -> do
    ext' <- uniquifyISpace ext
    m (p', ext')

-- | Do many binds.
binds :: (a -> (c -> x) -> x) -> [a] -> ([c] -> x) -> x
binds bind ps m = binds' ps mempty
  where
    binds' [] cs = m $ reverse cs
    binds' (a : as) cs =
      bind a $ \c -> binds' as (c : cs)
