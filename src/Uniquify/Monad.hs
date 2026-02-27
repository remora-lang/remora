module Uniquify.Monad where

import Control.Monad.RWS
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Prettyprinter
import Prop
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
    MonadState Tag m
  )

newtype UniquifyM a = UniquifyM {runUniquifyM :: RWS Env () Tag a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader Env,
      MonadState Tag
    )

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
  Pat NoInfo Text ->
  (Pat NoInfo VName -> m a) ->
  m a
withPatParam uniquifyType p@(PatId v t _ pos) m = do
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
withTypeParam (ArrayTypeParam v) m = do
  vname <- newVName v
  et_vname <- newVName $ "*" <> v
  s_vname <- newVName $ "*" <> v
  local
    ( \env ->
        env
          { vnameEnvArrayTypeVarBundles =
              M.insert v (ArrayTypeVarBundle vname et_vname s_vname) $
                vnameEnvArrayTypeVarBundles env
          }
    )
    $ m (ArrayTypeParam vname)

-- | Bind a source type parameter into a local environment.
withExtentParam :: (MonadUniquify m) => ExtentParam Text -> (ExtentParam VName -> m a) -> m a
withExtentParam p m = do
  vname <- newVName $ unExtentParam p
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
  (TypeParam Text, TypeExp Text) ->
  ((TypeParam VName, TypeExp VName) -> m a) ->
  m a
withType uniquifyTypeExp (p, t) m =
  withTypeParam p $ \p' -> do
    t' <- uniquifyTypeExp t
    m (p', t')

-- | Bind an index binding.
withExtent ::
  (MonadUniquify m) =>
  (Extent Text -> m (Extent VName)) ->
  SourcePos ->
  (ExtentParam Text, Extent Text) ->
  ((ExtentParam VName, Extent VName) -> m a) ->
  m a
withExtent uniquifyExtent pos (p, ext) m =
  withExtentParam p $ \p' -> do
    ext' <- uniquifyExtent ext
    m (p', ext')

-- | Do many binds.
binds :: (a -> (c -> x) -> x) -> [a] -> ([c] -> x) -> x
binds bind ps m = binds' ps mempty
  where
    binds' [] cs = m $ reverse cs
    binds' (a : as) cs =
      bind a $ \c -> binds' as (c : cs)
