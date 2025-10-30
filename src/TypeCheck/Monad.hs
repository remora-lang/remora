module TypeCheck.Monad where

import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.RWS
import Control.Monad.Trans.Except
import Data.Bifunctor
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Prettyprinter
import RemoraPrelude
import Substitute
import Symbolic qualified
import Syntax
import Util
import VName

-- | Environment that maps the various categories of source names to 'VName's.
data VNameEnv
  = VNameEnv
  { -- | 'Text' vars to 'VName's.
    vnameEnvVars :: Map Text VName,
    -- | 'Text' atom type vars to 'VName's.
    vnameEnvAtomTypeVars :: Map Text VName,
    -- | 'Text' array type vars to 'VName's.
    vnameEnvArrayTypeVars :: Map Text VName,
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
    bindEnvVars :: Map VName (Type VName),
    -- | Atom type vars to types.
    bindEnvAtomTypes :: Map VName (ScalarType VName),
    -- | Dim vars to 'Dim's.
    bindEnvDims :: Map VName (Dim VName),
    -- | Shape vars to 'Shape's.
    bindEnvShapes :: Map VName (Shape VName)
  }
  deriving (Eq, Show)

instance Semigroup BindEnv where
  BindEnv vs1 ts1 ds1 ss1 <> BindEnv vs2 ts2 ds2 ss2 =
    BindEnv (vs1 <> vs2) (ts1 <> ts2) (ds1 <> ds2) (ss1 <> ss2)

instance Monoid BindEnv where
  mempty = BindEnv mempty mempty mempty mempty

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
lookupVNameArrayTypeVar :: (MonadCheck m) => Text -> m VName
lookupVNameArrayTypeVar v =
  lookupEnv "text array type" v $ (M.!? v) . vnameEnvArrayTypeVars . envVNames

-- | Look-up a dim variable.
lookupVNameDimVar :: (MonadCheck m) => Text -> m VName
lookupVNameDimVar v =
  lookupEnv "text dim" v $ (M.!? v) . vnameEnvDimVars . envVNames

-- | Look-up a shape variable.
lookupVNameShapeVar :: (MonadCheck m) => Text -> m VName
lookupVNameShapeVar v =
  lookupEnv "text shape" v $ (M.!? v) . vnameEnvShapeVars . envVNames

-- | Look-up the type of a term variable.
lookupVarType :: (MonadCheck m) => VName -> m (Type VName)
lookupVarType v =
  lookupEnv "" v $ (M.!? v) . bindEnvVars . envBinds

-- | Look-up an atom type variable binding.
lookupAtomType :: (MonadCheck m) => VName -> m (Maybe (ScalarType VName))
lookupAtomType v =
  asks $ (M.!? v) . bindEnvAtomTypes . envBinds

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
  (Type Text -> m (Type VName)) ->
  (Text, Type Text) ->
  ((VName, Type VName) -> m a) ->
  m a
bindParam checkType (v, t) m = do
  vname <- newVName v
  t' <- checkType t
  Env vnames binds <- ask
  let vnames' = vnames {vnameEnvVars = M.insert v vname $ vnameEnvVars vnames}
      binds' = binds {bindEnvVars = M.insert vname t' $ bindEnvVars binds}
      env' = Env vnames' binds'
  local (const env') $ m (vname, t')

bindArrayParam ::
  (MonadCheck m) =>
  (Type Text -> m (Type VName)) ->
  (Text, ArrayType Text) ->
  ((VName, ArrayType VName) -> m a) ->
  m a
bindArrayParam checkType (v, t) m = do
  bindParam checkType (v, ArrayType t) $ \(v', t') -> do
    m (v', arrayTypeView t' (uncurry A))

-- | Bind a source parameter into a local environment.
bindParam' ::
  (MonadCheck m) =>
  (Text, Type VName) ->
  (VName -> m a) ->
  m a
bindParam' (v, t) m = do
  vname <- newVName v
  Env vnames binds <- ask
  let vnames' = vnames {vnameEnvVars = M.insert v vname $ vnameEnvVars vnames}
      binds' = binds {bindEnvVars = M.insert vname t $ bindEnvVars binds}
      env' = Env vnames' binds'
  local (const env') $ m vname

-- | Bind a source type parameter into a local environment.
bindTypeParam :: (MonadCheck m) => TVar Text -> (TVar VName -> m a) -> m a
bindTypeParam tvar m = do
  vname <- newVName $ unTVar tvar
  Env vnames binds <- ask
  let tvar' = (const vname) <$> tvar
      vnames' =
        case tvar of
          AtomTVar v ->
            vnames
              { vnameEnvAtomTypeVars =
                  M.insert v vname $ vnameEnvAtomTypeVars vnames
              }
          ArrayTVar v ->
            vnames
              { vnameEnvArrayTypeVars =
                  M.insert v vname $ vnameEnvArrayTypeVars vnames
              }
      env' = Env vnames' binds
  local (const env') $ m tvar'

-- | Bind a source type parameter into a local environment.
bindIdxParam :: (MonadCheck m) => IVar Text -> (IVar VName -> m a) -> m a
bindIdxParam ivar m = do
  vname <- newVName $ unIVar ivar
  Env vnames binds <- ask
  let ivar' = (const vname) <$> ivar
      vnames' =
        case ivar of
          DVar v ->
            vnames
              { vnameEnvDimVars =
                  M.insert v vname $ vnameEnvDimVars vnames
              }
          SVar v ->
            vnames
              { vnameEnvShapeVars =
                  M.insert v vname $ vnameEnvShapeVars vnames
              }
      env' = Env vnames' binds
  local (const env') $ m ivar'

-- | Bind a type binding.
bindType ::
  (MonadCheck m) =>
  (Type Text -> m (Type VName)) ->
  SourcePos ->
  (TVar Text, Type Text) ->
  ((TVar VName, Type VName) -> m a) ->
  m a
bindType checkType pos (tvar, t) m =
  bindTypeParam tvar $ \tvar' -> do
    t' <- checkType t
    Env vnames binds <- ask
    binds' <-
      case (tvar', t') of
        (AtomTVar v, ScalarType et) ->
          pure $
            binds
              { bindEnvAtomTypes = M.insert v et $ bindEnvAtomTypes binds
              }
        (ArrayTVar v, ArrayType (A et s)) ->
          pure $
            binds
              { bindEnvAtomTypes = M.insert v et $ bindEnvAtomTypes binds,
                bindEnvShapes = M.insert v s $ bindEnvShapes binds
              }
        _ ->
          throwErrorPos pos $
            T.unlines
              [ "Incompatible type binding:",
                prettyText tvar,
                prettyText t
              ]
    let env' = Env vnames binds'
    local (const env') $ m (tvar', t')

-- | Bind an index binding.
bindIdx ::
  (MonadCheck m) =>
  (Idx Text -> m (Idx VName)) ->
  SourcePos ->
  (IVar Text, Idx Text) ->
  ((IVar VName, Idx VName) -> m a) ->
  m a
bindIdx checkIdx pos (ivar, idx) m =
  bindIdxParam ivar $ \ivar' -> do
    idx' <- checkIdx idx
    Env vnames binds <- ask
    binds' <-
      case (ivar', idx') of
        (DVar v, Dim d) ->
          pure $
            binds
              { bindEnvDims = M.insert v d $ bindEnvDims binds
              }
        (SVar v, Shape s) ->
          pure $
            binds
              { bindEnvShapes = M.insert v s $ bindEnvShapes binds
              }
        _ ->
          throwErrorPos pos $
            T.unlines
              [ "Incompatible index binding:",
                prettyText ivar,
                prettyText idx
              ]
    let env' = Env vnames binds'
    local (const env') $ m (ivar', idx')

-- | Do many binds.
binds :: (a -> (c -> x) -> x) -> [a] -> ([c] -> x) -> x
binds bind ps m = binds' ps mempty
  where
    binds' [] cs = m $ reverse cs
    binds' (a : as) cs =
      bind a $ \c -> binds' as (c : cs)
