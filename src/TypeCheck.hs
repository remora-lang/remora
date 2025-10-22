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
import Syntax
import Util
import VName

data Env = Env
  { envVarMap :: Map Text VName,
    envTVarMap :: Map Text VName,
    envIVarMap :: Map Text VName,
    envTypeMap :: Map VName (Type VName)
  }

withPos :: (Pretty x) => SourcePos -> x -> Text
withPos pos x =
  T.unlines
    [ "At " <> prettyText (show pos),
      prettyText x
    ]

initEnv :: Env
initEnv = Env mempty mempty mempty mempty

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

lookupVName :: (MonadCheck m) => Text -> m VName
lookupVName v = lookupEnv v $ (M.!? v) . envVarMap

lookupTVar :: (MonadCheck m) => Text -> m VName
lookupTVar v = lookupEnv v $ (M.!? v) . envTVarMap

lookupTVar' :: (MonadCheck m) => TVar Text -> m (TVar VName)
lookupTVar' tvar = do
  vname <- lookupTVar (unTVar tvar)
  pure $ const vname <$> tvar

lookupIVar :: (MonadCheck m) => Text -> m VName
lookupIVar v = lookupEnv v $ (M.!? v) . envIVarMap

lookupIVar' :: (MonadCheck m) => IVar Text -> m (IVar VName)
lookupIVar' ivar = do
  vname <- lookupIVar (unIVar ivar)
  pure $ const vname <$> ivar

lookupType :: (MonadCheck m) => VName -> m (Type VName)
lookupType v = do
  mt <- asks $ (M.!? v) . envTypeMap
  case mt of
    Nothing -> throwError $ "Unknown type for var: " <> prettyText v
    Just t -> pure t

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

bindTypeParam :: (MonadCheck m) => TVar Text -> (TVar VName -> m a) -> m a
bindTypeParam tvar m = do
  vname <- newVName $ unTVar tvar
  local
    ( \env ->
        env {envTVarMap = M.insert (unTVar tvar) vname $ envTVarMap env}
    )
    (m $ const vname <$> tvar)

bindIdxParam :: (MonadCheck m) => IVar Text -> (IVar VName -> m a) -> m a
bindIdxParam ivar m = do
  vname <- newVName $ unIVar ivar
  local
    ( \env ->
        env {envIVarMap = M.insert (unIVar ivar) vname $ envIVarMap env}
    )
    (m $ const vname <$> ivar)

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

newtype CheckM a = CheckM {runCheckM :: ExceptT Error (RWS Env () Tag) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader Env,
      MonadState Tag,
      MonadError Error
    )

check ::
  (Monad m) =>
  Exp Unchecked Text ->
  Either Error (Prelude VName m, Exp Typed VName)
check e = fst $ evalRWS (runExceptT $ runCheckM $ withPrelude $ checkExp e) initEnv initTag

checkExp :: (MonadCheck m) => Exp Unchecked Text -> m (Exp Typed VName)
checkExp (Var v _ pos) = do
  vname <- lookupVName v
  t <- lookupType vname
  pure $ Var vname (Typed t) pos
checkExp expr@(Array ns as _ pos) = do
  as' <- mapM checkAtom as
  case as' of
    [] ->
      throwError $
        withPos pos $
          "Empty array constructed without type: " <> prettyText expr
    (a' : _) -> do
      unless (all (\a'' -> typeOf a'' == typeOf a') as') $
        throwError $
          withPos pos $
            "Atoms in array have different types: " <> prettyText expr
      unless (product ns == length as') $
        throwError $
          withPos pos $
            "Array shape doesn't match number of elements: " <> prettyText expr
      pure $ Array ns as' (Typed $ TArr (typeOf a') (intsToShape ns)) pos
checkExp expr@(EmptyArray ns t _ pos) = do
  t' <- checkType t
  unless (product ns == 0) $
    throwError $
      withPos pos $
        "Empty array has a non-empty shape: " <> prettyText expr
  pure $ EmptyArray ns t' (Typed $ TArr t' (intsToShape ns)) pos
checkExp expr@(Frame ns es _ pos) = do
  es' <- mapM checkExp es
  case es' of
    [] ->
      throwError $
        withPos pos $
          "Empty frame constructed without type: " <> prettyText expr
    (e' : _) -> do
      unless (all (\e'' -> typeOf e'' == typeOf e') es') $
        throwError $
          withPos pos $
            "Expressions in frame have different types: " <> prettyText expr
      unless (product ns == length es') $
        throwError $
          withPos pos $
            "Frame shape doesn't match number of elements: " <> prettyText expr
      pure $ Frame ns es' (Typed $ TArr (typeOf e') (intsToShape ns)) pos
checkExp expr@(EmptyFrame ns t _ pos) = do
  t' <- checkType t
  unless (product ns == 0) $
    throwError $
      withPos pos $
        "Empty frame has a non-empty shape: " <> prettyText expr
  pure $ EmptyFrame ns t' (Typed $ TArr t' (intsToShape ns)) pos
checkExp expr@(App f args _ pos) = do
  f' <- checkExp f
  args' <- mapM checkExp args
  case arrayifyType $ typeOf f' of
    TArr (pts :-> ret) frame_f -> do
      let check_args pt arg =
            case normShape (shapeOf arg) \\ normShape (shapeOf pt) of
              Nothing ->
                throwError $
                  withPos pos $
                    T.unlines
                      [ "Ill-shaped application:",
                        "Parameter type: " <> prettyText pt,
                        "Argument: " <> prettyText arg,
                        "in",
                        prettyText expr
                      ]
              Just frame_a -> pure frame_a
      frames <- zipWithM check_args pts args'
      let principal = maximumShape $ map (frame_f <>) frames
          ret' = TArr ret principal
      pure $ App f' args' (Typed (ret', principal)) pos
    _ ->
      throwError $
        withPos pos $
          "Expected an array of functions in application: " <> prettyText expr
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
                  throwError $
                    withPos pos $
                      T.unlines
                        [ "Ill-kinded application:",
                          "Parameter type: " <> prettyText pt,
                          "Argument: " <> prettyText t,
                          "in",
                          prettyText expr
                        ]
      void $ zipWithM check_args pts ts'
      let r' = substitute' (zip pts ts') r
      pure $ TApp f' ts' (Typed r') pos
    _ ->
      throwError $
        withPos pos $
          T.unlines
            [ "Expected a forall exprressions in type application:",
              prettyText expr
            ]
checkExp expr@(IApp f is _ pos) = do
  f' <- checkExp f
  is' <- mapM (mapIdx (fmap Dim . checkDim) (fmap Shape . checkShape)) is
  case typeOf f' of
    Prod pts r -> do
      let check_args (SVar v) (Dim d) =
            pure (SVar v, Shape $ ShapeDim d)
          check_args (SVar v) (Shape s) =
            pure (SVar v, Shape s)
          check_args (DVar v) (Shape s)
            | Just d <- coerceToDim s = pure (DVar v, Dim d)
          check_args pt i =
            throwError $
              withPos pos $
                T.unlines
                  [ "Ill-sorted application:",
                    "Parameter type: " <> prettyText pt,
                    "Argument: " <> prettyText i,
                    "in",
                    prettyText expr
                  ]
      (pts', is'') <- unzip <$> zipWithM check_args pts is'
      let r' = substitute' (zip pts' is'') r
      pure $ IApp f' is'' (Typed r') pos
    _ ->
      throwError $
        withPos pos $
          T.unlines
            [ "Expected a prod expressions in idx application:",
              prettyText expr
            ]
checkExp expr@(Unbox is x_e box body _ pos) = do
  binds bindIdxParam is $ \is' -> do
    let is'' = map unIVar is'
    box' <- checkExp box
    case typeOf box' of
      Exists ps t -> do
        let t' = flip substitute t $ M.fromList $ zip (map unIVar ps) is''
        bindParam (x_e, t') $ \x_e' -> do
          body' <- checkExp body
          case typeOf body' of
            TArr t_b shape_b ->
              pure $ Unbox is' x_e' box' body' (Typed $ TArr t_b (shapeOf box' <> shape_b)) pos
            _ ->
              throwError $
                withPos pos $
                  T.unlines
                    [ "Wrong body type for unbox",
                      prettyText expr
                    ]
      _ ->
        throwError $
          withPos pos $
            T.unlines
              [ "Expected an existentially typed expression in unbox:",
                prettyText expr
              ]

checkAtom :: (MonadCheck m) => Atom Unchecked Text -> m (Atom Typed VName)
checkAtom (Base b _ pos) =
  pure $ Base b (Typed $ typeOf b) pos
checkAtom (Lambda ps e _ pos) = do
  let (xs, pts) = unzip ps
  pts' <- mapM checkType pts
  binds bindParam (zip xs pts') $ \xs' -> do
    e' <- checkExp e
    pure $ Lambda (zip xs' pts') e' (Typed $ pts' :-> typeOf e') pos
checkAtom (TLambda ps e _ pos) =
  binds bindTypeParam ps $ \ps' -> do
    e' <- checkExp e
    pure $ TLambda ps' e' (Typed $ Forall ps' $ typeOf e') pos
checkAtom (ILambda ps e _ pos) =
  binds bindIdxParam ps $ \ps' -> do
    e' <- checkExp e
    pure $ ILambda ps' e' (Typed $ Prod ps' $ typeOf e') pos
checkAtom atom@(Box shapes e box_t pos) = do
  shapes' <- mapM checkShape shapes
  e' <- checkExp e
  box_t' <- checkType box_t
  case box_t' of
    Exists is t -> do
      let subst = M.fromList $ zip is shapes'
      unless (typeOf e' == substitute subst t) $
        throwError $
          withPos pos $
            T.unlines
              [ "Wrong box type.",
                "Expected:",
                prettyText $ typeOf e',
                "But got:",
                prettyText $ substitute subst t
              ]
      pure $ Box shapes' e' box_t' pos
    _ ->
      throwError $
        withPos pos $
          T.unlines
            [ "Non-existential box type:",
              prettyText atom
            ]

checkType :: (MonadCheck m) => Type Text -> m (Type VName)
checkType = fmap normType . checkType'
  where
    checkType' (TVar tvar) = TVar <$> lookupTVar' tvar
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
    checkType' (Prod pts t) =
      binds bindIdxParam pts $ \pts' ->
        Prod pts' <$> checkType t
    checkType' (Exists pts t) = do
      binds bindIdxParam pts $ \pts' -> do
        Exists pts' <$> checkType t

checkDim :: (MonadCheck m) => Dim Text -> m (Dim VName)
checkDim = fmap normDim . checkDim'
  where
    checkDim' (DimVar v) = DimVar <$> lookupIVar v
    checkDim' (DimN d) = pure $ DimN d
    checkDim' (Add ds) = Add <$> mapM checkDim' ds

checkShape :: (MonadCheck m) => Shape Text -> m (Shape VName)
checkShape = fmap normShape . checkShape'
  where
    checkShape' (ShapeVar v) = ShapeVar <$> lookupIVar v
    checkShape' (ShapeDim d) = ShapeDim <$> checkDim d
    checkShape' (Concat ss) = Concat <$> mapM checkShape' ss

withPrelude :: (MonadCheck m, Monad n) => m a -> m (Prelude VName n, a)
withPrelude m = checkPrelude prelude mempty
  where
    checkPrelude [] prelude' =
      (reverse prelude',) <$> m
    checkPrelude (p : ps) prelude' = do
      case p of
        PreludeVal f t v -> do
          t' <- checkType t
          bindParam (f, t') $ \f' ->
            checkPrelude ps (PreludeVal f' t' v : prelude')
        PreludeType tvar ->
          bindTypeParam tvar $ \tvar' -> do
            checkPrelude ps (PreludeType tvar' : prelude')
