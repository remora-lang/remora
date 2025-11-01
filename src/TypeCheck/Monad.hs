module TypeCheck.Monad where

import Control.Monad.Error.Class
import Control.Monad.RWS
import Control.Monad.Trans.Except
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
    bindEnvVars :: Map VName (ArrayType Info VName),
    -- | Atom type vars to types.
    bindEnvAtomTypes :: Map VName (AtomType Info VName),
    -- | Array type vars to types.
    bindEnvArrayTypes :: Map VName (ArrayType Info VName),
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
    Nothing -> throwError $ "Unknown " <> t <> " var lookup: " <> prettyText a
    Just b -> pure b

-- | Look-up a variable.
lookupVNameVar :: (MonadCheck m) => Text -> m VName
lookupVNameVar v =
  lookupEnv "text" v $ (M.!? v) . vnameEnvVars . envVNames

-- | Look-up an atom type variable.
lookupVNameAtomTypeVar :: (MonadCheck m) => Text -> m VName
lookupVNameAtomTypeVar v =
  lookupEnv "text atom type" v $ (M.!? v) . vnameEnvAtomTypeVars . envVNames

-- | Look-up an array type variable.
lookupVNameArrayTypeVarBundle :: (MonadCheck m) => Text -> m (ArrayTypeVarBundle VName)
lookupVNameArrayTypeVarBundle v =
  lookupEnv "text array type" v $ (M.!? v) . vnameEnvArrayTypeVarBundles . envVNames

-- | Look-up a dim variable.
lookupVNameDimVar :: (MonadCheck m) => Text -> m VName
lookupVNameDimVar v =
  lookupEnv "text dim" v $ (M.!? v) . vnameEnvDimVars . envVNames

-- | Look-up a shape variable.
lookupVNameShapeVar :: (MonadCheck m) => Text -> m VName
lookupVNameShapeVar v =
  lookupEnv "text shape" v $ (M.!? v) . vnameEnvShapeVars . envVNames

-- | Look-up the type of a term variable.
lookupVarType :: (MonadCheck m) => VName -> m (ArrayType Info VName)
lookupVarType v =
  lookupEnv "" v $ (M.!? v) . bindEnvVars . envBinds

-- | Look-up an atom type variable binding.
lookupAtomType :: (MonadCheck m) => VName -> m (Maybe (AtomType Info VName))
lookupAtomType v =
  asks $ (M.!? v) . bindEnvAtomTypes . envBinds

-- | Look-up an array type variable binding.
lookupArrayType :: (MonadCheck m) => VName -> m (Maybe (ArrayType Info VName))
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

-- | Bind a source parameter into a local environment.
bindParam ::
  (MonadCheck m) =>
  (ArrayType NoInfo Text -> m (ArrayType Info VName)) ->
  (Text, ArrayType NoInfo Text) ->
  ((VName, ArrayType Info VName) -> m a) ->
  m a
bindParam checkArrayType (v, t) m = do
  vname <- newVName v
  t' <- checkArrayType t
  Env vnames bs <- ask
  let vnames' = vnames {vnameEnvVars = M.insert v vname $ vnameEnvVars vnames}
      bs' = bs {bindEnvVars = M.insert vname t' $ bindEnvVars bs}
      env' = Env vnames' bs'
  local (const env') $ m (vname, t')

-- | Bind a source parameter into a local environment.
bindParam' ::
  (MonadCheck m) =>
  (Text, ArrayType Info VName) ->
  (VName -> m a) ->
  m a
bindParam' (v, t) m = do
  vname <- newVName v
  Env vnames bs <- ask
  let vnames' = vnames {vnameEnvVars = M.insert v vname $ vnameEnvVars vnames}
      bs' = bs {bindEnvVars = M.insert vname t $ bindEnvVars bs}
      env' = Env vnames' bs'
  local (const env') $ m vname

-- | Bind a source type parameter into a local environment.
bindTypeParam :: (MonadCheck m) => TypeParam Text -> (TypeParam VName -> m a) -> m a
bindTypeParam tvar m = do
  vname <- newVName $ unTypeParam tvar
  Env vnames bs <- ask
  vnames' <-
    case tvar of
      AtomTypeParam v ->
        pure $
          vnames
            { vnameEnvAtomTypeVars =
                M.insert v vname $ vnameEnvAtomTypeVars vnames
            }
      ArrayTypeParam v -> do
        et_vname <- newVName $ "et_" <> v
        s_vname <- newVName $ "s_" <> v
        pure $
          vnames
            { vnameEnvArrayTypeVarBundles =
                M.insert v (ArrayTypeVarBundle vname et_vname s_vname) $ vnameEnvArrayTypeVarBundles vnames
            }
  let tvar' = (const vname) <$> tvar
      env' = Env vnames' bs
  local (const env') $ m tvar'

-- | Bind a source type parameter into a local environment.
bindExtentParam :: (MonadCheck m) => ExtentParam Text -> (ExtentParam VName -> m a) -> m a
bindExtentParam ivar m = do
  vname <- newVName $ unExtentParam ivar
  Env vnames bs <- ask
  let ivar' = (const vname) <$> ivar
      vnames' =
        case ivar of
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
      env' = Env vnames' bs
  local (const env') $ m ivar'

-- | Bind a type binding.
bindType ::
  (MonadCheck m) =>
  (Type NoInfo Text -> m (Type Info VName)) ->
  SourcePos ->
  (TypeParam Text, Type NoInfo Text) ->
  ((TypeParam VName, Type Info VName) -> m a) ->
  m a
bindType checkType pos (tvar, t) m =
  bindTypeParam tvar $ \tvar' -> do
    t' <- checkType t
    Env vnames bs <- ask
    bs' <-
      case (tvar', t') of
        (AtomTypeParam v, AtomType et) ->
          pure $
            bs
              { bindEnvAtomTypes = M.insert v et $ bindEnvAtomTypes bs
              }
        (ArrayTypeParam v, ArrayType t) ->
          pure $
            bs
              { bindEnvArrayTypes = M.insert v t $ bindEnvArrayTypes bs
              }
        _ ->
          throwErrorPos pos $
            T.unlines
              [ "Incompatible type binding:",
                prettyText tvar,
                prettyText t
              ]
    let env' = Env vnames bs'
    local (const env') $ m (tvar', t')

-- | Bind an index binding.
bindExtent ::
  (MonadCheck m) =>
  (Extent Text -> m (Extent VName)) ->
  SourcePos ->
  (ExtentParam Text, Extent Text) ->
  ((ExtentParam VName, Extent VName) -> m a) ->
  m a
bindExtent checkExtent pos (ivar, extent) m =
  bindExtentParam ivar $ \ivar' -> do
    extent' <- checkExtent extent
    Env vnames bs <- ask
    bs' <-
      case (ivar', extent') of
        (DimParam v, Dim d) ->
          pure $
            bs
              { bindEnvDims = M.insert v d $ bindEnvDims bs
              }
        (ShapeParam v, Shape s) ->
          pure $
            bs
              { bindEnvShapes = M.insert v s $ bindEnvShapes bs
              }
        _ ->
          throwErrorPos pos $
            T.unlines
              [ "Incompatible index binding:",
                prettyText ivar,
                prettyText extent
              ]
    let env' = Env vnames bs'
    local (const env') $ m (ivar', extent')

-- | Do many binds.
binds :: (a -> (c -> x) -> x) -> [a] -> ([c] -> x) -> x
binds bind ps m = binds' ps mempty
  where
    binds' [] cs = m $ reverse cs
    binds' (a : as) cs =
      bind a $ \c -> binds' as (c : cs)
