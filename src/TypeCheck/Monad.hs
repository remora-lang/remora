module TypeCheck.Monad
  ( MonadCheck (..),
    CheckM (..),
    Error,
    initEnv,
    throwErrorPos,
    inscopeVar,
    inscopeAtomTypeVar,
    inscopeDimVar,
    inscopeShapeVar,
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

import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.Reader
import Control.Monad.Trans.Except
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Intrinsics
import Prettyprinter
import Prop
import Syntax
import Util
import VName

-- | Environment that tracks in-scope names.
data VNameEnv
  = VNameEnv
  { -- | vars .
    vnameEnvVars :: Set VName,
    -- | atom type vars.
    vnameEnvAtomTypeVars :: Set VName,
    -- | dim vars.
    vnameEnvDimVars :: Set VName,
    -- | shape vars.
    vnameEnvShapeVars :: Set VName
  }
  deriving (Eq, Show)

instance Semigroup VNameEnv where
  VNameEnv vs1 ats1 ds1 ss1 <> VNameEnv vs2 ats2 ds2 ss2 =
    VNameEnv (vs1 <> vs2) (ats1 <> ats2) (ds1 <> ds2) (ss1 <> ss2)

instance Monoid VNameEnv where
  mempty = VNameEnv mempty mempty mempty mempty

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
  { -- | Enviroment that tracks in-scope 'VName's.
    envVNames :: VNameEnv,
    -- | Environment that maps 'VName's (of various kinds) to their bindings.
    envBinds :: BindEnv
  }
  deriving (Eq, Show)

instance Semigroup Env where
  Env ns1 bs1 <> Env ns2 bs2 = Env (ns1 <> ns2) (bs1 <> bs2)

instance Monoid Env where
  mempty = Env mempty mempty

-- | The initial environment. Includes intrinsics.
initEnv :: Env
initEnv =
  Env
    { envVNames = mempty {vnameEnvVars = S.fromList $ M.keys intrinsics},
      envBinds = mempty {bindEnvVars = intrinsics}
    }

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
    MonadError Error m
  )

-- | The typechecker monad.
newtype CheckM a = CheckM {runCheckM :: ExceptT Error (Reader Env) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader Env,
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
  (env -> Bool) ->
  m ()
lookupEnv t a look =
  unlessM (asks look) $
    throwError $
      "Unknown " <> t <> " var: " <> prettyText a

-- | Is this variable in scope?
inscopeVar :: (MonadCheck m) => VName -> m ()
inscopeVar v =
  lookupEnv "text" v $ (S.member v) . vnameEnvVars . envVNames

-- | Is this atom type variable in scope?
inscopeAtomTypeVar :: (MonadCheck m) => VName -> m ()
inscopeAtomTypeVar v =
  lookupEnv "text atom type" v $ (S.member v) . vnameEnvAtomTypeVars . envVNames

-- | Is this dim variable in scope?
inscopeDimVar :: (MonadCheck m) => VName -> m ()
inscopeDimVar v =
  lookupEnv "text dim" v $ (S.member v) . vnameEnvDimVars . envVNames

-- | Is this shape variable in scope?
inscopeShapeVar :: (MonadCheck m) => VName -> m ()
inscopeShapeVar v =
  lookupEnv "text shape" v $ (S.member v) . vnameEnvShapeVars . envVNames

-- | Look-up the type of a term variable.
lookupVar :: (MonadCheck m) => VName -> m (ArrayType VName)
lookupVar vname = do
  mt <- asks $ (M.!? vname) . bindEnvVars . envBinds
  case mt of
    Nothing -> error $ "Unknown var: " <> prettyString vname
    Just t -> pure t

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
  (TypeExp VName -> m (TypeExp VName)) ->
  Pat NoInfo VName ->
  (Pat Info VName -> m a) ->
  m a
withPatParam checkType p@(PatId vname t _ pos) m = do
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
      localVNames (\vnames -> vnames {vnameEnvVars = S.insert vname $ vnameEnvVars vnames}) $
        localBinds (\bs -> bs {bindEnvVars = M.insert vname at $ bindEnvVars bs}) $
          m $
            PatId vname t' (Info at) pos

-- | Bind a source parameter into a local environment.
withParam ::
  (MonadCheck m) =>
  (t -> m (ArrayType VName)) ->
  (VName, t) ->
  ((VName, ArrayType VName) -> m a) ->
  m a
withParam checkArrayType (vname, t) m = do
  t' <- checkArrayType t
  localVNames (\vnames -> vnames {vnameEnvVars = S.insert vname $ vnameEnvVars vnames}) $
    localBinds (\bs -> bs {bindEnvVars = M.insert vname t' $ bindEnvVars bs}) $
      m (vname, t')

-- | Bind a source parameter into a local environment.
withParam' :: (MonadCheck m) => (VName, ArrayType VName) -> (VName -> m a) -> m a
withParam' (vname, t) f = withParam pure (vname, t) (f . fst)

-- | Bind a source type parameter into a local environment.
withTypeParam :: (MonadCheck m) => TypeParam VName -> (TypeParam VName -> m a) -> m a
withTypeParam (AtomTypeParam vname) m = do
  localVNames
    ( \vnames ->
        vnames
          { vnameEnvAtomTypeVars =
              S.insert vname $ vnameEnvAtomTypeVars vnames
          }
    )
    $ m (AtomTypeParam vname)
withTypeParam p@(ArrayTypeParam v) m =
  error $ "withTypeParam: should never happen: " <> prettyString p

-- | Bind a source type parameter into a local environment.
withExtentParam :: (MonadCheck m) => ExtentParam VName -> (ExtentParam VName -> m a) -> m a
withExtentParam p m =
  localVNames
    ( \vnames ->
        case p of
          DimParam v ->
            vnames
              { vnameEnvDimVars =
                  S.insert vname $ vnameEnvDimVars vnames
              }
          ShapeParam v ->
            vnames
              { vnameEnvShapeVars =
                  S.insert vname $ vnameEnvShapeVars vnames
              }
    )
    $ m (vname <$ p)
  where
    vname = unExtentParam p

-- | Bind a type binding.
withType ::
  (MonadCheck m) =>
  (TypeExp VName -> m (TypeExp VName)) ->
  (TypeParam VName, TypeExp VName) ->
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
  (Extent VName -> m (Extent VName)) ->
  SourcePos ->
  (ExtentParam VName, Extent VName) ->
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
