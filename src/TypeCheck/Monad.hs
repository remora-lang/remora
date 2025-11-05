module TypeCheck.Monad
  ( ArrayTypeVarBundle (..),
    MonadCheck (..),
    CheckM (..),
    Error,
    initEnv,
    throwErrorPos,
    fetchVar,
    fetchAtomTypeVar,
    fetchArrayTypeVar,
    fetchDimVar,
    fetchShapeVar,
    lookupVar,
    lookupAtomType,
    lookupArrayType,
    lookupDim,
    lookupShape,
    withPatParam,
    withParam,
    withParam',
    withTypeParam,
    withExtentParam,
    withType,
    withExtent,
    binds,
  )
where

import Control.Monad.Error.Class
import Control.Monad.RWS
import Control.Monad.Trans.Except
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
data VNameEnv
  = VNameEnv
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

instance Semigroup VNameEnv where
  VNameEnv vs1 ats1 ts1 ds1 ss1 <> VNameEnv vs2 ats2 ts2 ds2 ss2 =
    VNameEnv (vs1 <> vs2) (ats1 <> ats2) (ts1 <> ts2) (ds1 <> ds2) (ss1 <> ss2)

instance Monoid VNameEnv where
  mempty = VNameEnv mempty mempty mempty mempty mempty

-- | Environment that maps the various categories of things you can bind to
-- their bindings.
data BindEnv
  = BindEnv
  { -- | Term variables to their types.
    bindEnvVars :: Map VName (ArrayType VName),
    -- | Atom type vars to types.
    bindEnvAtomTypes :: Map VName (TypeExp VName),
    -- | Array type vars to types.
    bindEnvArrayTypes :: Map VName (TypeExp VName),
    -- | Dim vars to 'Dim's.
    bindEnvDims :: Map VName (Dim VName),
    -- | Shape vars to 'Shape's.
    bindEnvShapes :: Map VName (Shape VName)
  }
  deriving (Eq, Show)

instance Semigroup BindEnv where
  BindEnv vs1 ets1 ts1 ds1 ss1 <> BindEnv vs2 ets2 ts2 ds2 ss2 =
    BindEnv (vs1 <> vs2) (ets1 <> ets2) (ts1 <> ts2) (ds1 <> ds2) (ss1 <> ss2)

instance Monoid BindEnv where
  mempty = BindEnv mempty mempty mempty mempty mempty

-- | The typechecker environment.
data Env = Env
  { -- | Enviroment that maps 'Text' vars (of various kinds) to 'VName's.
    envVNames :: VNameEnv,
    -- | Environment that maps 'VName's (of various kinds) to their bindings.
    envBinds :: BindEnv
  }
  deriving (Eq, Show)

instance Semigroup Env where
  Env ns1 bs1 <> Env ns2 bs2 = Env (ns1 <> ns2) (bs1 <> bs2)

instance Monoid Env where
  mempty = Env mempty mempty

-- | The initial environment. (It's just empty.)
initEnv :: Env
initEnv = mempty

-- | Throw an error with source position information.
throwErrorPos :: (MonadError Text m) => SourcePos -> Text -> m a
throwErrorPos pos t =
  throwError $
    T.unlines
      [ prettyText pos <> ": error:",
        t
      ]

type Error = Text

type MonadCheck m =
  ( Monad m,
    MonadReader Env m,
    MonadState Tag m,
    MonadError Error m
  )

-- | The typechecker monad.
newtype CheckM a = CheckM {runCheckM :: ExceptT Error (RWS Env () Tag) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader Env,
      MonadState Tag,
      MonadError Error
    )

localVNames :: (MonadCheck m) => (VNameEnv -> VNameEnv) -> m a -> m a
localVNames f =
  local $ \env -> env {envVNames = f $ envVNames env}

localBinds :: (MonadCheck m) => (BindEnv -> BindEnv) -> m a -> m a
localBinds f =
  local $ \env -> env {envBinds = f $ envBinds env}

-- | Lookup something in the environment.
lookupEnv ::
  ( MonadReader env m,
    MonadError Error m,
    Pretty a
  ) =>
  Text ->
  a ->
  (env -> Maybe b) ->
  m b
lookupEnv t a look = do
  mb <- asks look
  case mb of
    Nothing -> throwError $ "Unknown " <> t <> " var: " <> prettyText a
    Just b -> pure b

-- | Fetch the unique name for a variable.
fetchVar :: (MonadCheck m) => Text -> m VName
fetchVar v =
  lookupEnv "text" v $ (M.!? v) . vnameEnvVars . envVNames

-- | Fetch the unique name for an atom type variable.
fetchAtomTypeVar :: (MonadCheck m) => Text -> m VName
fetchAtomTypeVar v =
  lookupEnv "text atom type" v $ (M.!? v) . vnameEnvAtomTypeVars . envVNames

-- | Fetch the unique name for an array type variable.
fetchArrayTypeVar :: (MonadCheck m) => Text -> m (ArrayTypeVarBundle VName)
fetchArrayTypeVar v =
  lookupEnv "text array type" v $ (M.!? v) . vnameEnvArrayTypeVarBundles . envVNames

-- | Fetch the unique name for a dim variable.
fetchDimVar :: (MonadCheck m) => Text -> m VName
fetchDimVar v =
  lookupEnv "text dim" v $ (M.!? v) . vnameEnvDimVars . envVNames

-- | Fetch the unique name for a shape variable.
fetchShapeVar :: (MonadCheck m) => Text -> m VName
fetchShapeVar v =
  lookupEnv "text shape" v $ (M.!? v) . vnameEnvShapeVars . envVNames

-- | Look-up the type of a term variable.
lookupVar :: (MonadCheck m) => VName -> m (ArrayType VName)
lookupVar v =
  lookupEnv "" v $ (M.!? v) . bindEnvVars . envBinds

-- | Look-up an atom type variable binding.
lookupAtomType :: (MonadCheck m) => VName -> m (Maybe (TypeExp VName))
lookupAtomType v =
  asks $ (M.!? v) . bindEnvAtomTypes . envBinds

-- | Look-up an array type variable binding.
lookupArrayType :: (MonadCheck m) => VName -> m (Maybe (TypeExp VName))
lookupArrayType v =
  asks $ (M.!? v) . bindEnvArrayTypes . envBinds

-- | Look-up a dim variable binding.
lookupDim :: (MonadCheck m) => VName -> m (Maybe (Dim VName))
lookupDim v =
  asks $ (M.!? v) . bindEnvDims . envBinds

-- | Look-up a shape variable binding.
lookupShape :: (MonadCheck m) => VName -> m (Maybe (Shape VName))
lookupShape v =
  asks $ (M.!? v) . bindEnvShapes . envBinds

withPatParam ::
  (MonadCheck m) =>
  (TypeExp Text -> m (TypeExp VName)) ->
  Pat NoInfo Text ->
  (Pat Info VName -> m a) ->
  m a
withPatParam checkType p@(PatId v t _ pos) m = do
  vname <- newVName v
  t' <- checkType t
  case convertArrayTypeExp t' of
    Nothing ->
      throwErrorPos pos $
        T.unlines
          [ "Expected array",
            prettyText p,
            T.pack $ show t
          ]
    Just at ->
      localVNames (\vnames -> vnames {vnameEnvVars = M.insert v vname $ vnameEnvVars vnames}) $
        localBinds (\bs -> bs {bindEnvVars = M.insert vname at $ bindEnvVars bs}) $
          m $
            PatId vname t' (Info at) pos

-- | Bind a source parameter into a local environment.
withParam ::
  (MonadCheck m) =>
  (t -> m (ArrayType VName)) ->
  (Text, t) ->
  ((VName, ArrayType VName) -> m a) ->
  m a
withParam checkArrayType (v, t) m = do
  vname <- newVName v
  t' <- checkArrayType t
  localVNames (\vnames -> vnames {vnameEnvVars = M.insert v vname $ vnameEnvVars vnames}) $
    localBinds (\bs -> bs {bindEnvVars = M.insert vname t' $ bindEnvVars bs}) $
      m (vname, t')

-- | Bind a source parameter into a local environment.
withParam' :: (MonadCheck m) => (Text, ArrayType VName) -> (VName -> m a) -> m a
withParam' (v, t) f = withParam pure (v, t) (f . fst)

-- | Bind a source type parameter into a local environment.
withTypeParam :: (MonadCheck m) => TypeParam Text -> (TypeParam VName -> m a) -> m a
withTypeParam (AtomTypeParam v) m = do
  vname <- newVName v
  localVNames
    ( \vnames ->
        vnames
          { vnameEnvAtomTypeVars =
              M.insert v vname $ vnameEnvAtomTypeVars vnames
          }
    )
    $ m (AtomTypeParam vname)
withTypeParam (ArrayTypeParam v) m = do
  vname <- newVName v
  et_vname <- newVName $ "*" <> v
  s_vname <- newVName $ "*" <> v
  localVNames
    ( \vnames ->
        vnames
          { vnameEnvArrayTypeVarBundles =
              M.insert v (ArrayTypeVarBundle vname et_vname s_vname) $
                vnameEnvArrayTypeVarBundles vnames
          }
    )
    $ m (ArrayTypeParam vname)

-- | Bind a source type parameter into a local environment.
withExtentParam :: (MonadCheck m) => ExtentParam Text -> (ExtentParam VName -> m a) -> m a
withExtentParam p m = do
  vname <- newVName $ unExtentParam p
  localVNames
    ( \vnames ->
        case p of
          DimParam v ->
            vnames
              { vnameEnvDimVars =
                  M.insert v vname $ vnameEnvDimVars vnames
              }
          ShapeParam v ->
            vnames
              { vnameEnvShapeVars =
                  M.insert v vname $ vnameEnvShapeVars vnames
              }
    )
    $ m (vname <$ p)

-- | Bind a type binding.
withType ::
  (MonadCheck m) =>
  (TypeExp Text -> m (TypeExp VName)) ->
  (TypeParam Text, TypeExp Text) ->
  ((TypeParam VName, TypeExp VName) -> m a) ->
  m a
withType checkTypeExp (p, t) m =
  withTypeParam p $ \p' -> do
    t' <- checkTypeExp t
    f <- case p' of
      AtomTypeParam v ->
        pure $ \bs ->
          bs {bindEnvAtomTypes = M.insert v t' $ bindEnvAtomTypes bs}
      ArrayTypeParam v ->
        pure $ \bs ->
          bs {bindEnvArrayTypes = M.insert v t' $ bindEnvArrayTypes bs}
      _ ->
        throwErrorPos (posOf t) $
          T.unlines
            [ "Incompatible type binding:",
              prettyText p,
              prettyText t
            ]
    localBinds f $ m (p', t')

-- | Bind an index binding.
withExtent ::
  (MonadCheck m) =>
  (Extent Text -> m (Extent VName)) ->
  SourcePos ->
  (ExtentParam Text, Extent Text) ->
  ((ExtentParam VName, Extent VName) -> m a) ->
  m a
withExtent checkExtent pos (p, ext) m =
  withExtentParam p $ \p' -> do
    ext' <- checkExtent ext
    f <- case (p', ext') of
      (DimParam v, Dim d) ->
        pure $
          \bs -> bs {bindEnvDims = M.insert v d $ bindEnvDims bs}
      (ShapeParam v, Shape s) ->
        pure $
          \bs -> bs {bindEnvShapes = M.insert v s $ bindEnvShapes bs}
      _ ->
        throwErrorPos pos $
          T.unlines
            [ "Incompatible extent binding:",
              prettyText p,
              prettyText ext
            ]
    localBinds f $ m (p', ext')

-- | Do many binds.
binds :: (a -> (c -> x) -> x) -> [a] -> ([c] -> x) -> x
binds bind ps m = binds' ps mempty
  where
    binds' [] cs = m $ reverse cs
    binds' (a : as) cs =
      bind a $ \c -> binds' as (c : cs)
