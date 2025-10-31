module TypeCheck (check) where

import Control.Monad
import Control.Monad.RWS
import Control.Monad.Trans.Except
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Prop
import RemoraPrelude
import Substitute
import Symbolic qualified
import Syntax
import TypeCheck.Monad
import Util
import VName

-- | Type check a program.
check ::
  (Monad m) =>
  Exp NoInfo Text ->
  Either Error (Prelude Info VName m, Exp Info VName)
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

-- | Check a 'Dim'.
checkDim :: (MonadCheck m) => Dim Text -> m (Dim VName)
checkDim = fmap normDim . checkDim'
  where
    checkDim' (DimVar v) = do
      v' <- lookupVNameDimVar v
      md <- lookupDim v'
      case md of
        Nothing -> pure $ DimVar v'
        Just d -> pure d
    checkDim' (DimN d) = pure $ DimN d
    checkDim' (Add ds) = Add <$> mapM checkDim' ds
    checkDim' (Mul ds) = Mul <$> mapM checkDim' ds

-- | Check a 'Shape'.
checkShape :: (MonadCheck m) => Shape Text -> m (Shape VName)
checkShape = fmap normShape . checkShape'
  where
    checkShape' (ShapeVar v) = do
      v' <- lookupVNameShapeVar v
      ms <- lookupShape v'
      case ms of
        Nothing -> pure $ ShapeVar v'
        Just s -> pure s
    checkShape' (ShapeDim d) = ShapeDim <$> checkDim d
    checkShape' (Concat ss) = Concat <$> mapM checkShape' ss

-- | Check an `Idx`.
checkIdx :: (MonadCheck m) => Idx Text -> m (Idx VName)
checkIdx = mapIdx (fmap Dim . checkDim) (fmap Shape . checkShape)

-- | Check a 'Type'.
checkType :: (MonadCheck m) => Type NoInfo Text -> m (Type Info VName)
checkType = fmap normType . checkType'
  where
    checkType' (ArrayType t) = ArrayType <$> checkArrayType t
    checkType' (ScalarType t) = ScalarType <$> checkScalarType t

checkArrayType :: (MonadCheck m) => ArrayType NoInfo Text -> m (ArrayType Info VName)
checkArrayType (A t shape) =
  A <$> checkScalarType t <*> checkShape shape
checkArrayType (ArrayTypeVar v _ _) = do
  ArrayTypeVarBundle v' et_v' s_v' <- lookupVNameArrayTypeVarBundle v
  mt <- lookupArrayType v'
  case mt of
    Nothing -> pure $ ArrayTypeVar v' (Info et_v') (Info s_v')
    Just t -> pure t

checkScalarType :: (MonadCheck m) => ScalarType NoInfo Text -> m (ScalarType Info VName)
checkScalarType (ScalarTVar v) = do
  v' <- lookupVNameAtomTypeVar v
  mt <- lookupAtomType v'
  case mt of
    Nothing -> pure $ ScalarTVar v'
    Just t -> pure t
checkScalarType Bool = pure Bool
checkScalarType Int = pure Int
checkScalarType Float = pure Float
checkScalarType (as :-> b) =
  (:->) <$> mapM checkArrayType as <*> checkArrayType b
checkScalarType (Forall pts t) =
  binds bindTypeParam pts $ \pts' ->
    Forall pts' <$> checkArrayType t
checkScalarType (Pi pts t) =
  binds bindIdxParam pts $ \pts' ->
    Pi pts' <$> checkArrayType t
checkScalarType (Sigma pts t) = do
  binds bindIdxParam pts $ \pts' -> do
    Sigma pts' <$> checkArrayType t

-- | Type check an unchecked 'Exp'.
checkExp :: (MonadCheck m) => Exp NoInfo Text -> m (Exp Info VName)
checkExp (Var v _ pos) = do
  vname <- lookupVNameVar v
  t <- lookupVarType vname
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
      let et = scalarTypeOf a'
      pure $ Array ns as' (Info $ A et (intsToShape ns)) pos
checkExp expr@(EmptyArray ns t _ pos) = do
  t' <- checkScalarType t
  unless (product ns == 0) $
    throwErrorPos pos $
      "Empty array has a non-empty shape: " <> prettyText expr
  pure $ EmptyArray ns t' (Info $ A t' (intsToShape ns)) pos
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
      let A et s = arrayTypeOf e'
      pure $ Frame ns es' (Info $ A et ((intsToShape ns) <> s)) pos
checkExp expr@(EmptyFrame ns t _ pos) = do
  t' <- checkScalarType t
  unless (product ns == 0) $
    throwErrorPos pos $
      "Empty frame has a non-empty shape: " <> prettyText expr
  pure $ EmptyFrame ns t' (Info $ A t' (intsToShape ns)) pos
checkExp expr@(App f args _ pos) = do
  f' <- checkExp f
  args' <- mapM checkExp args
  case arrayifyType $ typeOf f' of
    ArrayType (A (pts :-> ret) frame_f) -> do
      let check_args pt arg = do
            let pt_elem = arrayTypeScalar pt
                arg_elem = elemType $ typeOf arg
            unlessM (pt_elem ~= arg_elem) $
              throwErrorPos pos $
                T.unlines
                  [ "Ill-typed application:",
                    "Parameter elem type: " <> prettyText pt_elem,
                    "Argument elem type: " <> prettyText arg_elem,
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
          ret' = A (arrayTypeScalar ret) (principal <> arrayTypeShape ret)
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
    ArrayType (A (Forall pts r) frame_f) -> do
      let check_args (atom_subst, shape_subst) (pt, t) =
            case (pt, t) of
              (AtomTVar v, ScalarType et) ->
                pure (M.insert v et atom_subst, shape_subst)
              (ArrayTVar v, ArrayType (A et s)) ->
                pure (M.insert v et atom_subst, M.insert v s shape_subst)
              _ ->
                throwErrorPos pos $
                  T.unlines
                    [ "Ill-kinded application:",
                      "Parameter type: " <> prettyText pt,
                      "Argument: " <> prettyText t,
                      "in",
                      prettyText expr
                    ]
      (atom_subst, shape_subst) <- foldM check_args mempty $ zip pts ts'
      let A rt rshape = substitute shape_subst $ substitute atom_subst r
          r' = A rt (frame_f <> rshape)
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
    ArrayType (A (Pi pts r) frame_f) -> do
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
      let A rt shape = substitute' (zip pts' is'') r
          r' = A rt (frame_f <> shape)
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
      ArrayType (A (Sigma ps t) frame_f) -> do
        let t' = flip substitute' t $ zip (map unIVar ps) is''
        bindParam' (x_e, t') $ \x_e' -> do
          body' <- checkExp body
          case typeOf body' of
            ArrayType (A t_b shape_b) ->
              pure $
                Unbox
                  is'
                  x_e'
                  box'
                  body'
                  (Info $ A t_b (frame_f <> shape_b))
                  pos
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
    pure $ Let bs' e' (Info $ arrayTypeOf e') pos
  where
    checkMaybeType ::
      (MonadCheck m) =>
      Maybe (ArrayType NoInfo Text) -> m (Maybe (ArrayType Info VName))
    checkMaybeType Nothing = pure Nothing
    checkMaybeType (Just t) = Just <$> checkArrayType t

    checkAnnot ::
      (MonadCheck m) =>
      ArrayType Info VName -> Maybe (ArrayType Info VName) -> m ()
    checkAnnot t mannot =
      case mannot of
        Nothing -> pure ()
        Just annot -> do
          unlessM (t ~= annot) $
            throwErrorPos pos $
              T.unlines
                [ "Type:",
                  prettyText t,
                  "doesn't match the annotated type:",
                  prettyText annot
                ]

    withBind :: (MonadCheck m) => Bind NoInfo Text -> (Bind Info VName -> m a) -> m a
    withBind (BindVal v mt ve pos) m = do
      ve' <- checkExp ve
      let t = arrayTypeOf ve'
      mt' <- checkMaybeType mt
      checkAnnot t mt'
      bindParam' (v, t) $ \vname ->
        m $ BindVal vname mt' ve' pos
    withBind (BindFun f params mt body pos) m = do
      mt' <- checkMaybeType mt
      (params', body') <-
        binds (bindParam checkArrayType) params $ \params' -> do
          body' <- checkExp body
          checkAnnot (arrayTypeOf body') mt'
          pure (params', body')
      bindParam' (f, A (map snd params' :-> arrayTypeOf body') mempty) $ \f' ->
        m $ BindFun f' params' mt' body' pos
    withBind (BindTFun f params mt body pos) m =
      binds bindTypeParam params $ \params' -> do
        body' <- checkExp body
        mt' <- checkMaybeType mt
        let body_t = arrayTypeOf body'
        checkAnnot body_t mt'
        bindParam' (f, A (Forall params' body_t) mempty) $ \f' ->
          m $ BindTFun f' params' mt' body' pos
    withBind (BindIFun f params mt body pos) m =
      binds bindIdxParam params $ \params' -> do
        body' <- checkExp body
        mt' <- checkMaybeType mt
        let body_t = arrayTypeOf body'
        checkAnnot body_t mt'
        bindParam' (f, A (Pi params' body_t) mempty) $ \f' ->
          m $ BindIFun f' params' mt' body' pos
    withBind (BindType tvar t pos) m =
      bindType checkType pos (tvar, t) $ \(tvar', t') ->
        m $ BindType tvar' t' pos
    withBind (BindIdx ivar idx pos) m =
      bindIdx checkIdx pos (ivar, idx) $ \(ivar', idx') ->
        m $ BindIdx ivar' idx' pos

-- | Type check an unchecked 'Atom'.
checkAtom :: (MonadCheck m) => Atom NoInfo Text -> m (Atom Info VName)
checkAtom (Base b _ pos) =
  pure $ Base b (Info $ baseTypeOf b) pos
checkAtom (Lambda params e _ pos) = do
  binds (bindParam checkArrayType) params $ \params' -> do
    e' <- checkExp e
    let r = arrayTypeOf e'
    pure $ Lambda params' e' (Info $ map snd params' :-> r) pos
checkAtom (TLambda ps e _ pos) =
  binds bindTypeParam ps $ \ps' -> do
    e' <- checkExp e
    let r = arrayTypeOf e'
    pure $ TLambda ps' e' (Info $ Forall ps' r) pos
checkAtom (ILambda ps e _ pos) =
  binds bindIdxParam ps $ \ps' -> do
    e' <- checkExp e
    let r = arrayTypeOf e'
    pure $ ILambda ps' e' (Info $ Pi ps' r) pos
checkAtom atom@(Box idx e box_t pos) = do
  idx' <- mapM checkIdx idx
  e' <- checkExp e
  box_t' <- checkScalarType box_t
  case box_t' of
    Sigma is t -> do
      let t' = substitute' (zip is idx') t
      unlessM (arrayTypeOf e' ~= t') $
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
withPrelude :: (MonadCheck m, Monad n) => m a -> m (Prelude Info VName n, a)
withPrelude m = checkPrelude prelude mempty
  where
    checkPrelude [] prelude' =
      (reverse prelude',) <$> m
    checkPrelude (PreludeVal f t v : ps) prelude' = do
      bindParam checkArrayType (f, t) $ \(f', t') ->
        checkPrelude ps (PreludeVal f' t' v : prelude')
