module TypeCheck (check) where

import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.RWS
import Control.Monad.Trans.Except
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

-- | Type check a program.
check ::
  (Monad m) =>
  Exp NoInfo Text ->
  Either Error (Prelude VName m, Exp Info VName)
check e =
  fst $
    evalRWS
      ( runExceptT $
          runCheckM $
            withPrelude $
              checkExp e
      )
      initEnv
      initTag

-- | The typechecker environment.
data Env = Env
  { -- | Source var names to 'VName's.
    envVarMap :: Map Text VName,
    -- | Source type var names to 'VName's.
    envTVarMap :: Map Text VName,
    -- | Source idx var names to 'VName's.
    envIVarMap :: Map Text VName,
    -- | Term variables to their types.
    envTypeMap :: Map VName (Type VName),
    -- | Type vars to 'Type's. Needed for 'Let'. (TODO: fix.)
    envTVarTypeMap :: Map (TVar VName) (Type VName),
    -- | Dim vars to 'Dim's. Needed for 'Let'. (TODO: fix.)
    envDimMap :: Map VName (Dim VName),
    -- | Shape vars to 'Dim's. Needed for 'Let'. (TODO: fix.)
    envShapeMap :: Map VName (Shape VName)
  }

-- | The initial environment. (It's just empty.)
initEnv :: Env
initEnv = Env mempty mempty mempty mempty mempty mempty mempty

-- | Throw an error with source position information.
throwErrorPos :: (MonadError Text m) => SourcePos -> Text -> m a
throwErrorPos pos t =
  throwError $
    T.unlines
      [ prettyText pos <> ": error:",
        t
      ]

-- | Lookup something in the environment.
lookupEnv ::
  ( MonadReader env m,
    MonadError Error m,
    Pretty a
  ) =>
  a ->
  (env -> Maybe b) ->
  m b
lookupEnv a look = do
  mb <- asks look
  case mb of
    Nothing -> throwError $ "Unknown lookup: " <> prettyText a
    Just b -> pure b

-- | Lookup a source variable's 'VName'.
lookupVName :: (MonadCheck m) => Text -> m VName
lookupVName v = lookupEnv v $ (M.!? v) . envVarMap

-- | Lookup a source type var's 'VName'.
lookupTVar :: (MonadCheck m) => Text -> m VName
lookupTVar v = lookupEnv v $ (M.!? v) . envTVarMap

lookupTVar' :: (MonadCheck m) => TVar Text -> m (TVar VName)
lookupTVar' tvar = do
  vname <- lookupTVar (unTVar tvar)
  pure $ vname <$ tvar

-- | Lookup a source idx var's 'VName'.
lookupIVar :: (MonadCheck m) => Text -> m VName
lookupIVar v = lookupEnv v $ (M.!? v) . envIVarMap

-- | Lookup a variable's type.
lookupType :: (MonadCheck m) => VName -> m (Type VName)
lookupType v = do
  mt <- asks $ (M.!? v) . envTypeMap
  case mt of
    Nothing -> throwError $ "Unknown type for var: " <> prettyText v
    Just t -> pure t

-- | Bind a source parameter into a local environment.
bindParam :: (MonadCheck m) => (Text, Type VName) -> (VName -> m a) -> m a
bindParam (v, t) m = do
  vname <- newVName v
  local
    ( \env ->
        env
          { envVarMap = M.insert v vname $ envVarMap env,
            envTypeMap = M.insert vname t $ envTypeMap env
          }
    )
    (m vname)

checkBindParam :: (MonadCheck m) => (Text, Type Text) -> ((VName, Type VName) -> m a) -> m a
checkBindParam (v, t) m = do
  t' <- checkType t
  vname <- newVName v
  local
    ( \env ->
        env
          { envVarMap = M.insert v vname $ envVarMap env,
            envTypeMap = M.insert vname t' $ envTypeMap env
          }
    )
    (m (vname, t'))

-- | Bind a source type parameter into a local environment.
bindTypeParam :: (MonadCheck m) => TVar Text -> (TVar VName -> m a) -> m a
bindTypeParam tvar m = do
  vname <- newVName $ unTVar tvar
  local
    ( \env ->
        env {envTVarMap = M.insert (unTVar tvar) vname $ envTVarMap env}
    )
    (m $ vname <$ tvar)

-- | Bind a source idx parameter into a local environment.
bindIdxParam :: (MonadCheck m) => IVar Text -> (IVar VName -> m a) -> m a
bindIdxParam ivar m = do
  vname <- newVName $ unIVar ivar
  local
    ( \env ->
        env {envIVarMap = M.insert (unIVar ivar) vname $ envIVarMap env}
    )
    (m $ vname <$ ivar)

-- | Do many binds.
binds :: (a -> (c -> x) -> x) -> [a] -> ([c] -> x) -> x
binds bind ps m = binds' ps mempty
  where
    binds' [] cs = m $ reverse cs
    binds' (a : as) cs =
      bind a $ \c -> binds' as (c : cs)

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

-- | Type equality according to Chapter 4 of Justin's thesis.
(~=) :: (MonadCheck m) => Type VName -> Type VName -> m Bool
TArr t s ~= TArr y x =
  (t ~= y) ^&& pure (s Symbolic.@= x)
(ps :-> r) ~= (qs :-> t)
  | length ps == length qs =
      andM (zipWith (~=) ps qs) ^&& (r ~= t)
Forall ps r ~= Forall qs t
  | length ps == length qs = do
      xs <- forM ps $ \p -> do
        vname <- newVName $ prettyText p
        pure $ vname <$ p
      substitute' (zip ps xs) r ~= substitute' (zip qs xs) t
Pi ps r ~= Pi qs t
  | length ps == length qs = do
      xs <- forM ps $ \p -> do
        vname <- newVName $ prettyText p
        pure $ vname <$ p
      substitute' (zip ps xs) r ~= substitute' (zip qs xs) t
Sigma ps r ~= Sigma qs t
  | length ps == length qs = do
      xs <- forM ps $ \p -> do
        vname <- newVName $ prettyText p
        pure $ vname <$ p
      substitute' (zip ps xs) r ~= substitute' (zip qs xs) t
t ~= r = pure $ t == r

infix 4 ~=

-- | Check a 'Dim'.
checkDim :: (MonadCheck m) => Dim Text -> m (Dim VName)
checkDim = fmap normDim . checkDim'
  where
    checkDim' (DimVar v) = do
      -- fix this
      v' <- lookupIVar v
      md <- asks $ (M.!? v') . envDimMap
      case md of
        Nothing -> pure $ DimVar v'
        Just d -> pure d
    checkDim' (DimN d) = pure $ DimN d
    checkDim' (Add ds) = Add <$> mapM checkDim' ds

-- | Check a 'Shape'.
checkShape :: (MonadCheck m) => Shape Text -> m (Shape VName)
checkShape = fmap normShape . checkShape'
  where
    checkShape' (ShapeVar v) = do
      -- fix this
      v' <- lookupIVar v
      ms <- asks $ (M.!? v') . envShapeMap
      case ms of
        Nothing -> pure $ ShapeVar v'
        Just s -> pure s
    checkShape' (ShapeDim d) = ShapeDim <$> checkDim d
    checkShape' (Concat ss) = Concat <$> mapM checkShape' ss

-- | Check an `Idx`.
checkIdx :: (MonadCheck m) => Idx Text -> m (Idx VName)
checkIdx = mapIdx (fmap Dim . checkDim) (fmap Shape . checkShape)

-- | Check a 'Type'.
checkType :: (MonadCheck m) => Type Text -> m (Type VName)
checkType = fmap normType . checkType'
  where
    checkType' (TVar tvar) = do
      -- fix this
      tvar' <- lookupTVar' tvar
      mt <- asks $ (M.!? tvar') . envTVarTypeMap
      case mt of
        Nothing -> pure $ TVar tvar'
        Just t -> pure t
    checkType' Bool = pure Bool
    checkType' Int = pure Int
    checkType' Float = pure Float
    checkType' (TArr t shape) =
      TArr <$> checkType' t <*> checkShape shape
    checkType' (as :-> b) =
      (:->) <$> mapM checkType' as <*> checkType' b
    checkType' (Forall pts t) =
      binds bindTypeParam pts $ \pts' ->
        Forall pts' <$> checkType t
    checkType' (Pi pts t) =
      binds bindIdxParam pts $ \pts' ->
        Pi pts' <$> checkType t
    checkType' (Sigma pts t) = do
      binds bindIdxParam pts $ \pts' -> do
        Sigma pts' <$> checkType t

-- | Type check an unchecked 'Exp'.
checkExp :: (MonadCheck m) => Exp NoInfo Text -> m (Exp Info VName)
checkExp (Var v _ pos) = do
  vname <- lookupVName v
  t <- lookupType vname
  pure $ Var vname (Info t) pos
checkExp expr@(Array ns as _ pos) = do
  as' <- mapM checkAtom as
  case as' of
    [] ->
      throwErrorPos pos $
        "Empty array constructed without type: " <> prettyText expr
    (a' : _) -> do
      unlessM (allM (\a'' -> typeOf a'' ~= typeOf a') as') $
        throwErrorPos pos $
          "Atoms in array have different types: " <> prettyText expr
      unless (product ns == length as') $
        throwErrorPos pos $
          "Array shape doesn't match number of elements: " <> prettyText expr
      let t = typeOf a'
      unless (atomKind t) $
        throwErrorPos pos $
          "Non-atom-kinded array elements of type: " <> prettyText t
      pure $ Array ns as' (Info $ TArr t (intsToShape ns)) pos
checkExp expr@(EmptyArray ns t _ pos) = do
  t' <- checkType t
  unless (product ns == 0) $
    throwErrorPos pos $
      "Empty array has a non-empty shape: " <> prettyText expr
  unless (atomKind t') $
    throwErrorPos pos $
      "Non-atom-kinded array elements of type: " <> prettyText t'
  pure $ EmptyArray ns t' (Info $ TArr t' (intsToShape ns)) pos
checkExp expr@(Frame ns es _ pos) = do
  es' <- mapM checkExp es
  case es' of
    [] ->
      throwErrorPos pos $
        "Empty frame constructed without type: " <> prettyText expr
    (e' : _) -> do
      unlessM (allM (\e'' -> typeOf e'' ~= typeOf e') es') $
        throwErrorPos pos $
          "Expressions in frame have different types: " <> prettyText expr
      unless (product ns == length es') $
        throwErrorPos pos $
          "Frame shape doesn't match number of elements: " <> prettyText expr
      let t = typeOf e'
      -- TODO: fix
      -- unless (arrayKind t) $
      --  throwErrorPos pos $
      --    "Non-array-kinded frame elements of type: " <> prettyText t
      pure $ Frame ns es' (Info $ TArr t (intsToShape ns)) pos
checkExp expr@(EmptyFrame ns t _ pos) = do
  t' <- checkType t
  unless (product ns == 0) $
    throwErrorPos pos $
      "Empty frame has a non-empty shape: " <> prettyText expr
  unless (arrayKind t') $
    throwErrorPos pos $
      "Non-array-kinded frame elements of type: " <> prettyText t'
  pure $ EmptyFrame ns t' (Info $ TArr t' (intsToShape ns)) pos
checkExp expr@(App f args _ pos) = do
  f' <- checkExp f
  args' <- mapM checkExp args
  case arrayifyType $ typeOf f' of
    TArr (pts :-> ret) frame_f -> do
      let check_args pt arg = do
            unlessM (elemType (normType pt) ~= elemType (typeOf arg)) $
              throwErrorPos pos $
                T.unlines
                  [ "Ill-typed application:",
                    "Parameter elem type: " <> prettyText (elemType (normType pt)),
                    "Argument elem type: " <> prettyText (elemType (typeOf arg)),
                    "in",
                    prettyText expr
                  ]

            case shapeOf arg \\ shapeOf pt of
              Nothing ->
                throwErrorPos pos $
                  T.unlines
                    [ "Ill-shaped application:",
                      "Parameter type: " <> prettyText pt,
                      "Argument: " <> prettyText arg,
                      "in",
                      prettyText expr
                    ]
              Just frame_a -> pure frame_a
      frames <- zipWithM check_args pts args'
      let principal = Symbolic.maximumShape $ frame_f : frames
          ret' = TArr ret principal
      pure $ App f' args' (Info (ret', principal)) pos
    t ->
      throwErrorPos pos $
        T.unlines
          [ "Expected an array of functions in application: ",
            prettyText expr,
            prettyText t
          ]
checkExp expr@(TApp f ts _ pos) = do
  f' <- checkExp f
  ts' <- mapM checkType ts
  case typeOf f' of
    Forall pts r -> do
      let check_args pt t =
            let same_kind =
                  case pt of
                    AtomTVar {} -> atomKind
                    ArrayTVar {} -> arrayKind
             in unless (same_kind t) $
                  throwErrorPos pos $
                    T.unlines
                      [ "Ill-kinded application:",
                        "Parameter type: " <> prettyText pt,
                        "Argument: " <> prettyText t,
                        "in",
                        prettyText expr
                      ]
      zipWithM_ check_args pts ts'
      let r' = substitute' (zip pts ts') r
      pure $ TApp f' ts' (Info r') pos
    _ ->
      throwErrorPos pos $
        T.unlines
          [ "Expected a forall exprressions in type application:",
            prettyText expr
          ]
checkExp expr@(IApp f is _ pos) = do
  f' <- checkExp f
  is' <- mapM (mapIdx (fmap Dim . checkDim) (fmap Shape . checkShape)) is
  case typeOf f' of
    Pi pts r -> do
      let check_args (SVar v) (Dim d) =
            pure (SVar v, Shape $ ShapeDim d)
          check_args (SVar v) (Shape s) =
            pure (SVar v, Shape s)
          check_args (DVar v) (Shape s)
            | Just d <- coerceToDim s = pure (DVar v, Dim d)
          check_args (DVar v) (Dim d) =
            pure (DVar v, Dim d)
          check_args pt i =
            throwErrorPos pos $
              T.unlines
                [ "Ill-sorted application:",
                  "Parameter type: " <> prettyText pt,
                  "Argument: " <> prettyText i,
                  "in",
                  prettyText expr
                ]
      (pts', is'') <- unzip <$> zipWithM check_args pts is'
      let r' = substitute' (zip pts' is'') r
      pure $ IApp f' is'' (Info r') pos
    _ ->
      throwErrorPos pos $
        T.unlines
          [ "Expected a prod expressions in idx application:",
            prettyText expr
          ]
checkExp expr@(Unbox is x_e box body _ pos) = do
  binds bindIdxParam is $ \is' -> do
    let is'' = map unIVar is'
    box' <- checkExp box
    case typeOf box' of
      Sigma ps t -> do
        let t' = flip substitute' t $ zip (map unIVar ps) is''
        bindParam (x_e, t') $ \x_e' -> do
          body' <- checkExp body
          case typeOf body' of
            TArr t_b shape_b ->
              pure $ Unbox is' x_e' box' body' (Info $ TArr t_b (shapeOf box' <> shape_b)) pos
            _ ->
              throwErrorPos pos $
                T.unlines
                  [ "Wrong body type for unbox:",
                    prettyText expr
                  ]
      _ ->
        throwErrorPos pos $
          T.unlines
            [ "Expected an existentially typed expression in unbox:",
              prettyText expr
            ]
checkExp (Let bs e _ pos) = do
  binds withBind bs $ \bs' -> do
    e' <- checkExp e
    pure $ Let bs' e' (Info $ typeOf e') pos
  where
    withBind :: (MonadCheck m) => Bind NoInfo Text -> (Bind Info VName -> m a) -> m a
    withBind (BindVal v t ve) m = do
      ve' <- checkExp ve
      t' <- checkType t
      bindParam (v, t') $ \vname -> m $ BindVal vname t' ve'
    withBind (BindFun f params ret_t body) m = do
      ret_t' <- checkType ret_t
      binds checkBindParam params $ \params' -> do
        body' <- checkExp body
        bindParam (f, map snd params' :-> ret_t') $ \f' -> do
          m $ BindFun f' params' ret_t' body'
    withBind (BindType tvar t) m = do
      bindTypeParam tvar $ \tvar' -> do
        t' <- checkType t
        local
          ( \env ->
              env {envTVarTypeMap = M.insert tvar' t' $ envTVarTypeMap env}
          )
          (m $ BindType tvar' t')
    withBind (BindIdx ivar idx) m = do
      bindIdxParam ivar $ \ivar' -> do
        idx' <- checkIdx idx
        local
          ( \env ->
              case idx' of
                Dim d -> env {envDimMap = M.insert (unIVar ivar') d $ envDimMap env}
                Shape s -> env {envShapeMap = M.insert (unIVar ivar') s $ envShapeMap env}
          )
          (m $ BindIdx ivar' idx')

-- | Type check an unchecked 'Atom'.
checkAtom :: (MonadCheck m) => Atom NoInfo Text -> m (Atom Info VName)
checkAtom (Base b _ pos) =
  pure $ Base b (Info $ typeOf b) pos
checkAtom (Lambda ps e _ pos) = do
  let (xs, pts) = unzip ps
  pts' <- mapM checkType pts
  binds bindParam (zip xs pts') $ \xs' -> do
    e' <- checkExp e
    pure $ Lambda (zip xs' pts') e' (Info $ pts' :-> typeOf e') pos
checkAtom (TLambda ps e _ pos) =
  binds bindTypeParam ps $ \ps' -> do
    e' <- checkExp e
    pure $ TLambda ps' e' (Info $ Forall ps' $ typeOf e') pos
checkAtom (ILambda ps e _ pos) =
  binds bindIdxParam ps $ \ps' -> do
    e' <- checkExp e
    pure $ ILambda ps' e' (Info $ Pi ps' $ typeOf e') pos
checkAtom atom@(Box idx e box_t pos) = do
  idx' <- mapM checkIdx idx
  e' <- checkExp e
  box_t' <- checkType box_t
  case box_t' of
    Sigma is t -> do
      let t' = substitute' (zip is idx') t
      unlessM (typeOf e' ~= t') $
        throwErrorPos pos $
          T.unlines
            [ "Wrong box type.",
              "Expected:",
              prettyText $ typeOf e',
              "But got:",
              prettyText t'
            ]
      pure $ Box idx' e' box_t' pos
    _ ->
      throwErrorPos pos $
        T.unlines
          [ "Non-existential box type:",
            prettyText atom
          ]

-- | Binds prelude bindings into the local environment.
withPrelude :: (MonadCheck m, Monad n) => m a -> m (Prelude VName n, a)
withPrelude m = checkPrelude prelude mempty
  where
    checkPrelude [] prelude' =
      (reverse prelude',) <$> m
    checkPrelude (PreludeVal f t v : ps) prelude' = do
      t' <- checkType t
      bindParam (f, t') $ \f' ->
        checkPrelude ps (PreludeVal f' t' v : prelude')
