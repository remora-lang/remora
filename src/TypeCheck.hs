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

-- | Check a 'Dim'.
checkDim :: (MonadCheck m) => Dim Text -> m (Dim VName)
checkDim = fmap normDim . checkDim'
  where
    checkDim' (DimVar v) = do
      v' <- fetchDimVar v
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
      v' <- fetchShapeVar v
      ms <- lookupShape v'
      case ms of
        Nothing -> pure $ ShapeVar v'
        Just s -> pure s
    checkShape' (ShapeDim d) = ShapeDim <$> checkDim d
    checkShape' (Concat ss) = Concat <$> mapM checkShape' ss

-- | Check an `Extent`.
checkExtent :: (MonadCheck m) => Extent Text -> m (Extent VName)
checkExtent = mapExtent (fmap Dim . checkDim) (fmap Shape . checkShape)

-- | Check a 'Type'.
checkType :: (MonadCheck m) => Type Text -> m (Type VName)
checkType = fmap normType . checkType'
  where
    checkType' (ArrayType t) = ArrayType <$> checkArrayType t
    checkType' (AtomType t) = AtomType <$> checkAtomType t

checkArrayTypeExp :: (MonadCheck m) => TypeExp Text -> m (ArrayType VName)
checkArrayTypeExp t = do
  t' <- checkTypeExp t
  case convertArrayTypeExp t' of
    Nothing ->
      throwErrorPos (posOf t) $
        T.unlines
          [ "Expected an array-kinded type:",
            prettyText t
          ]
    Just at -> pure at

checkAtomTypeExp :: (MonadCheck m) => TypeExp Text -> m (AtomType VName)
checkAtomTypeExp t = do
  t' <- checkTypeExp t
  case convertAtomTypeExp t' of
    Nothing ->
      throwErrorPos (posOf t) $
        T.unlines
          [ "Expected an atom-kinded type:",
            prettyText t
          ]
    Just at -> pure at

checkArrayType :: (MonadCheck m) => ArrayType Text -> m (ArrayType VName)
checkArrayType (A t shape) =
  A <$> checkAtomType t <*> checkShape shape

checkAtomType :: (MonadCheck m) => AtomType Text -> m (AtomType VName)
checkAtomType (AtomTypeVar v) =
  AtomTypeVar <$> fetchAtomTypeVar v
checkAtomType Bool = pure Bool
checkAtomType Int = pure Int
checkAtomType Float = pure Float
checkAtomType (as :-> b) =
  (:->) <$> mapM checkArrayType as <*> checkArrayType b
checkAtomType (Forall pts t) =
  binds withTypeParam pts $ \pts' ->
    Forall pts' <$> checkArrayType t
checkAtomType (Pi pts t) =
  binds withExtentParam pts $ \pts' ->
    Pi pts' <$> checkArrayType t
checkAtomType (Sigma pts t) = do
  binds withExtentParam pts $ \pts' -> do
    Sigma pts' <$> checkArrayType t

-- | Type check an unchecked 'Exp'.
checkExp :: (MonadCheck m) => Exp NoInfo Text -> m (Exp Info VName)
checkExp (Var v _ pos) = do
  vname <- fetchVar v
  t <- lookupVar vname
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
  t' <- checkTypeExp t
  unless (product ns == 0) $
    throwErrorPos pos $
      "Empty array has a non-empty shape: " <> prettyText expr
  case convertAtomTypeExp t' of
    Nothing ->
      throwErrorPos pos $
        "Non-scalar kinded type annotation on empty arra."
    Just et -> pure $ EmptyArray ns t' (Info $ A et (intsToShape ns)) pos
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
  t' <- checkTypeExp t
  unless (product ns == 0) $
    throwErrorPos pos $
      "Empty frame has a non-empty shape: " <> prettyText expr
  case convertAtomTypeExp t' of
    Nothing ->
      throwErrorPos pos $
        "Non-scalar kinded type annotation on empty arra."
    Just et -> pure $ EmptyFrame ns t' (Info $ A et (intsToShape ns)) pos
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
                    "Param elem type: " <> prettyText pt_elem,
                    "Argument elem type: " <> prettyText arg_elem,
                    "in",
                    prettyText expr
                  ]

            case shapeOf arg Symbolic.\\ shapeOf pt of
              Nothing ->
                throwErrorPos pos $
                  T.unlines
                    [ "Ill-shaped application:",
                      "Param type: " <> prettyText pt,
                      "Argument: " <> prettyText arg,
                      "Argument type: " <> prettyText (arrayTypeOf arg),
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
  ts' <- mapM checkTypeExp ts
  case typeOf f' of
    ArrayType (A (Forall pts r) frame_f) -> do
      let check_args (atom_subst, shape_subst) (pt, t) =
            case (pt, convertTypeExp t) of
              (AtomTypeParam v, Just (AtomType et)) ->
                pure (M.insert v et atom_subst, shape_subst)
              (ArrayTypeParam v, Just (ArrayType (A et s))) ->
                pure (M.insert v et atom_subst, M.insert v s shape_subst)
              _ ->
                throwErrorPos pos $
                  T.unlines
                    [ "Ill-kinded application:",
                      "Param type: " <> prettyText pt,
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
  is' <- mapM (mapExtent (fmap Dim . checkDim) (fmap Shape . checkShape)) is
  case typeOf f' of
    ArrayType (A (Pi pts r) frame_f) -> do
      let check_args (ShapeParam v) (Dim d) =
            pure (ShapeParam v, Shape $ ShapeDim d)
          check_args (ShapeParam v) (Shape s) =
            pure (ShapeParam v, Shape s)
          check_args (DimParam v) (Shape s)
            | Just d <- coerceToDim s = pure (DimParam v, Dim d)
          check_args (DimParam v) (Dim d) =
            pure (DimParam v, Dim d)
          check_args pt i =
            throwErrorPos pos $
              T.unlines
                [ "Ill-sorted application:",
                  "Param type: " <> prettyText pt,
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
          [ "Expected a prod expressions in extent application:",
            prettyText expr
          ]
checkExp expr@(Unbox is x_e box body _ pos) = do
  binds withExtentParam is $ \is' -> do
    let is'' = map unExtentParam is'
    box' <- checkExp box
    case typeOf box' of
      ArrayType (A (Sigma ps t) frame_f) -> do
        let t' = flip substitute' t $ zip (map unExtentParam ps) is''
        withParam' (x_e, t') $ \x_e' -> do
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
    checkMaybeTypeExp ::
      (MonadCheck m) =>
      Maybe (TypeExp Text) -> m (Maybe (TypeExp VName))
    checkMaybeTypeExp Nothing = pure Nothing
    checkMaybeTypeExp (Just t) = Just <$> checkTypeExp t

    checkAnnot ::
      (MonadCheck m) =>
      ArrayType VName -> Maybe (TypeExp VName) -> m ()
    checkAnnot t mannot =
      case mannot of
        Nothing -> pure ()
        Just annot -> do
          case convertArrayTypeExp annot of
            Just at ->
              unlessM (t ~= at) $
                throwErrorPos pos $
                  T.unlines
                    [ "Type:",
                      prettyText t,
                      "doesn't match the annotated type:",
                      prettyText annot
                    ]
            Nothing ->
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
      mt' <- checkMaybeTypeExp mt
      checkAnnot t mt'
      withParam' (v, t) $ \vname ->
        m $ BindVal vname mt' ve' pos
    withBind (BindFun f params mt body _ pos) m = do
      mt' <- checkMaybeTypeExp mt
      (params', body') <-
        binds (withPatParam checkTypeExp) params $ \params' -> do
          body' <- checkExp body
          checkAnnot (arrayTypeOf body') mt'
          pure (params', body')
      let t = map arrayTypeOf params' :-> arrayTypeOf body'
      withParam' (f, mkScalarArrayType t) $ \f' ->
        m $ BindFun f' params' mt' body' (Info t) pos
    withBind (BindTFun f params mt body _ pos) m =
      binds withTypeParam params $ \params' -> do
        body' <- checkExp body
        mt' <- checkMaybeTypeExp mt
        let body_t = arrayTypeOf body'
        checkAnnot body_t mt'
        let t = Forall params' body_t
        withParam' (f, mkScalarArrayType t) $ \f' ->
          m $ BindTFun f' params' mt' body' (Info t) pos
    withBind (BindIFun f params mt body _ pos) m =
      binds withExtentParam params $ \params' -> do
        body' <- checkExp body
        mt' <- checkMaybeTypeExp mt
        let body_t = arrayTypeOf body'
        checkAnnot body_t mt'
        let t = Pi params' body_t
        withParam' (f, mkScalarArrayType t) $ \f' ->
          m $ BindIFun f' params' mt' body' (Info t) pos
    withBind (BindType tvar t _ pos) m =
      withType checkTypeExp (tvar, t) $ \(tvar', t') ->
        case convertTypeExp t' of
          Nothing -> throwErrorPos pos $ "Invalid type."
          Just t'' -> m $ BindType tvar' t' (Info t'') pos
    withBind (BindExtent ivar extent pos) m =
      withExtent checkExtent pos (ivar, extent) $ \(ivar', extent') ->
        m $ BindExtent ivar' extent' pos

-- | Type check an unchecked 'Atom'.
checkAtom :: (MonadCheck m) => Atom NoInfo Text -> m (Atom Info VName)
checkAtom (Base b _ pos) =
  pure $ Base b (Info $ baseTypeOf b) pos
checkAtom (Lambda params e _ pos) = do
  binds (withPatParam checkTypeExp) params $ \params' -> do
    e' <- checkExp e
    let r = arrayTypeOf e'
    pure $ Lambda params' e' (Info $ map arrayTypeOf params' :-> r) pos
checkAtom (TLambda ps e _ pos) =
  binds withTypeParam ps $ \ps' -> do
    e' <- checkExp e
    let r = arrayTypeOf e'
    pure $ TLambda ps' e' (Info $ Forall ps' r) pos
checkAtom (ILambda ps e _ pos) =
  binds withExtentParam ps $ \ps' -> do
    e' <- checkExp e
    let r = arrayTypeOf e'
    pure $ ILambda ps' e' (Info $ Pi ps' r) pos
checkAtom atom@(Box extent e box_t _ pos) = do
  extent' <- mapM checkExtent extent
  e' <- checkExp e
  box_t' <- checkTypeExp box_t
  case convertAtomTypeExp box_t' of
    Just bt@(Sigma is t) -> do
      let t' = substitute' (zip is extent') t
      unlessM (arrayTypeOf e' ~= t') $
        throwErrorPos pos $
          T.unlines
            [ "Wrong box type.",
              "Expected:",
              prettyText $ typeOf e',
              "But got:",
              prettyText t'
            ]
      pure $ Box extent' e' box_t' (Info bt) pos
    _ ->
      throwErrorPos pos $
        T.unlines
          [ "Non-existential box type:",
            prettyText atom
          ]

checkTypeExp :: (MonadCheck m) => TypeExp Text -> m (TypeExp VName)
checkTypeExp (TEAtomVar v pos) = do
  vname <- fetchAtomTypeVar v
  mt <- lookupAtomType vname
  case mt of
    Nothing -> pure $ TEAtomVar vname pos
    Just t -> pure t
checkTypeExp (TEArrayVar v pos) = do
  ArrayTypeVarBundle vname et_vname s_vname <- fetchArrayTypeVar v
  mt <- lookupArrayType vname
  case mt of
    Nothing -> pure $ TEArrayVar vname pos
    Just t -> pure t
checkTypeExp (TEBool pos) = pure $ TEBool pos
checkTypeExp (TEInt pos) = pure $ TEInt pos
checkTypeExp (TEFloat pos) = pure $ TEFloat pos
checkTypeExp (TEArray t s pos) =
  TEArray <$> checkTypeExp t <*> checkShape s <*> pure pos
checkTypeExp (TEArrow ts r pos) =
  TEArrow <$> mapM checkTypeExp ts <*> checkTypeExp r <*> pure pos
checkTypeExp (TEForall params t pos) =
  binds withTypeParam params $ \params' ->
    TEForall params' <$> checkTypeExp t <*> pure pos
checkTypeExp (TEPi params t pos) =
  binds withExtentParam params $ \params' ->
    TEPi params' <$> checkTypeExp t <*> pure pos
checkTypeExp (TESigma params t pos) = do
  binds withExtentParam params $ \params' -> do
    TESigma params' <$> checkTypeExp t <*> pure pos

-- | Binds prelude bindings into the local environment.
withPrelude :: (MonadCheck m, Monad n) => m a -> m (Prelude VName n, a)
withPrelude m = checkPrelude prelude mempty
  where
    checkPrelude [] prelude' =
      (reverse prelude',) <$> m
    checkPrelude (PreludeVal f t v : ps) prelude' = do
      withParam checkArrayType (f, t) $ \(f', t') ->
        checkPrelude ps (PreludeVal f' t' v : prelude')
