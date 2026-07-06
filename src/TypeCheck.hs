module TypeCheck (check, checkExp) where

import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.Reader
import Control.Monad.Trans.Except
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Text qualified as T
import Pass
import Prop
import Substitute
import Symbolic qualified
import Syntax
import TypeCheck.Monad
import Util
import VName

-- | Type check a program.
check :: UniqueProg -> PassM Prog
check p =
  liftEither $
    runReader
      ( runExceptT $
          runCheckM $
            checkProg p
      )
      initEnv

checkExp :: UniqueExp -> PassM Exp
checkExp e =
  liftEither $
    runReader
      ( runExceptT $
          runCheckM $
            checkExp' e
      )
      initEnv

checkProg :: (MonadCheck m) => UniqueProg -> m Prog
checkProg (Prog decs) = binds withDecl decs $ pure . Prog

withDecl :: (MonadCheck m) => UniqueDecl -> (Decl -> m a) -> m a
withDecl (Def b) m = withBind b $ m . Def
withDecl (Entry f params mt body _ pos) m = do
  mt' <- checkMaybeTypeExp mt
  (params', body') <-
    binds (withPatParam checkTypeExp) params $ \params' -> do
      body' <- checkExp' body
      checkAnnot (arrayTypeOf body') mt' pos
      pure (params', body')
  let t = arrowType (map arrayTypeOf params') (arrayTypeOf body')
  withParam' (f, mkScalarArrayType t) $ \f' ->
    m $ Entry f' params' mt' body' (Info t) pos

-- | Check a 'Dim'.
checkDim :: (MonadCheck m) => Dim VName -> m (Dim VName)
checkDim = fmap normDim . checkDim'
  where
    checkDim' (DimVar vname) = do
      inscopeDimVar vname
      md <- lookupDim vname
      case md of
        Nothing -> pure $ DimVar vname
        Just d -> pure d
    checkDim' (DimN d) = pure $ DimN d
    checkDim' (Add ds) = Add <$> mapM checkDim' ds
    checkDim' (Sub ds) = Sub <$> mapM checkDim' ds
    checkDim' (Mul ds) = Mul <$> mapM checkDim' ds

-- | Check a 'Shape'.
checkShape :: (MonadCheck m) => Shape VName -> m (Shape VName)
checkShape = fmap normShape . checkShape'
  where
    checkShape' (ShapeVar vname) = do
      inscopeShapeVar vname
      ms <- lookupShape vname
      case ms of
        Nothing -> pure $ ShapeVar vname
        Just s -> pure s
    checkShape' (ShapeDim d) = ShapeDim <$> checkDim d
    checkShape' (Concat ss) = Concat <$> mapM checkShape' ss

-- | Check an `ISpace`.
checkISpace :: (MonadCheck m) => ISpace VName -> m (ISpace VName)
checkISpace = mapISpace (fmap Dim . checkDim) (fmap Shape . checkShape)

-- | Type check an unchecked 'Exp'.
checkExp' :: (MonadCheck m) => UniqueExp -> m Exp
checkExp' (Var vname _ pos) = do
  inscopeVar vname
  t <- lookupVar vname
  pure $ Var vname (Info t) pos
checkExp' expr@(Array ns as _ pos) = do
  as' <- mapM checkAtom as
  let a' = NE.head as'
  unless (all (\a'' -> typeOf a'' ~= typeOf a') as') $
    throwErrorPos pos $
      "Atoms in array have different types: " <> prettyText expr
  unless (product ns == length as') $
    throwErrorPos pos $
      "Array shape doesn't match number of elements: " <> prettyText expr
  let et = scalarTypeOf a'
  pure $ Array ns as' (Info $ A et (intsToShape ns)) pos
checkExp' expr@(EmptyArray ns te _ pos) = do
  te' <- checkTypeExp te
  unless (product ns == 0) $
    throwErrorPos pos $
      "Empty array has a non-empty shape: " <> prettyText expr
  case convertArrayTypeExp te' of
    Nothing ->
      throwErrorPos pos "Non-scalar kinded type annotation on empty array."
    Just t -> pure $ EmptyArray ns te' (Info t) pos
checkExp' expr@(Frame ns es _ pos) = do
  es' <- mapM checkExp' es
  let e' = NE.head es'
  unless (all (\e'' -> typeOf e'' ~= typeOf e') es') $
    throwErrorPos pos $
      "Expressions in frame have different types: " <> prettyText expr
  unless (product ns == length es') $
    throwErrorPos pos $
      "Frame shape doesn't match number of elements: " <> prettyText expr
  let A et s = arrayTypeOf e'
  pure $ flattenExp $ Frame ns es' (Info $ A et (intsToShape ns <> s)) pos
checkExp' expr@(EmptyFrame ns te _ pos) = do
  te' <- checkTypeExp te
  unless (product ns == 0) $
    throwErrorPos pos $
      "Empty frame has a non-empty shape: " <> prettyText expr
  case convertArrayTypeExp te' of
    Nothing ->
      throwErrorPos pos "Non-scalar kinded type annotation on empty frame."
    Just t -> pure $ EmptyFrame ns te' (Info t) pos
checkExp' expr@(App f arg _ pos) = do
  f' <- checkExp' f
  arg' <- checkExp' arg
  case arrayifyType $ typeOf f' of
    ArrayType (A (pt :-> ret) frame_f) -> do
      let pt_elem = arrayTypeAtom pt
          arg_elem = elemType $ typeOf arg'
      unless (pt_elem ~= arg_elem) $
        throwErrorPos pos $
          T.unlines
            [ "Ill-typed application:",
              "Param elem type: " <> prettyText pt_elem,
              "Argument elem type: " <> prettyText arg_elem,
              "in",
              prettyText expr
            ]
      frame_a <- case shapeOf arg' Symbolic.\\ shapeOf pt of
        Nothing ->
          throwErrorPos pos $
            T.unlines
              [ "Ill-shaped application:",
                "Param type: " <> prettyText pt,
                "Argument: " <> prettyText arg',
                "Argument type: " <> prettyText (arrayTypeOf arg'),
                "in",
                prettyText expr
              ]
        Just fa -> pure fa
      case Symbolic.maximumShape [frame_f, frame_a] of
        Nothing ->
          throwErrorPos pos $
            T.unlines
              [ "Ill-shaped application:",
                prettyText expr
              ]
        Just principal ->
          let ret' = A (arrayTypeAtom ret) (principal <> arrayTypeShape ret)
           in pure $ App f' arg' (Info (ret', principal)) pos
    t ->
      throwErrorPos pos $
        T.unlines
          [ "Expected an array of functions in application: ",
            prettyText expr,
            prettyText t
          ]
checkExp' expr@(TApp f t _ pos) = do
  f' <- checkExp' f
  t' <- checkTypeExp t
  case typeOf f' of
    ArrayType (A (Forall pt r) frame_f) -> do
      atom_subst <- case (pt, convertTypeExp t') of
        (AtomTypeParam v, Just (AtomType et)) ->
          pure (M.singleton v et :: M.Map VName (AtomType VName))
        _ ->
          throwErrorPos pos $
            T.unlines
              [ "Ill-kinded application:",
                "Param type: " <> prettyText pt,
                "Argument: " <> prettyText t',
                "in",
                prettyText expr
              ]
      let A rt rshape = substitute (substAtomVars atom_subst) r
          r' = A rt (frame_f <> rshape)
      pure $ TApp f' t' (Info r') pos
    _ ->
      throwErrorPos pos $
        T.unlines
          [ "Expected a forall expression in type application:",
            prettyText expr
          ]
checkExp' expr@(IApp f i _ pos) = do
  f' <- checkExp' f
  i' <- mapISpace (fmap Dim . checkDim) (fmap Shape . checkShape) i
  case typeOf f' of
    ArrayType (A (Pi pt r) frame_f) -> do
      let check_arg (ShapeParam v) (Dim d) =
            pure (ShapeParam v, Shape $ ShapeDim d)
          check_arg (ShapeParam v) (Shape s) =
            pure (ShapeParam v, Shape s)
          check_arg (DimParam v) (Shape s)
            | Just d <- coerceToDim s = pure (DimParam v, Dim d)
          check_arg (DimParam v) (Dim d) = pure (DimParam v, Dim d)
          check_arg p ix =
            throwErrorPos pos $
              T.unlines
                [ "Ill-sorted application:",
                  "Param type: " <> prettyText p,
                  "Argument: " <> prettyText ix,
                  "in",
                  prettyText expr
                ]
      (pt', i'') <- check_arg pt i'
      let A rt shape = substitute (substISpaceVar (unISpaceParam pt') i'') r
          r' = A rt (frame_f <> shape)
      pure $ IApp f' i'' (Info r') pos
    _ ->
      throwErrorPos pos $
        T.unlines
          [ "Expected a pi expression in ispace application:",
            prettyText expr
          ]
checkExp' expr@(Unbox ep x_e box body _ pos) = do
  withISpaceParam ep $ \ep' -> do
    let i'' = unISpaceParam ep'
    box' <- checkExp' box
    case typeOf box' of
      ArrayType (A (Sigma ps t) frame_f) -> do
        let t' = substitute (renameVar (unISpaceParam ps) i'') t
        withParam' (x_e, t') $ \x_e' -> do
          body' <- checkExp' body
          case typeOf body' of
            ArrayType (A t_b shape_b) ->
              pure $
                Unbox
                  ep'
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
checkExp' (Let bs e _ pos) = do
  binds withBind (NE.toList bs) $ \bs' -> do
    e' <- checkExp' e
    pure $ Let (NE.fromList bs') e' (Info $ arrayTypeOf e') pos

checkMaybeTypeExp ::
  (MonadCheck m) =>
  Maybe (TypeExp VName) -> m (Maybe (TypeExp VName))
checkMaybeTypeExp Nothing = pure Nothing
checkMaybeTypeExp (Just t) = Just <$> checkTypeExp t

checkAnnot ::
  (MonadCheck m) =>
  ArrayType VName -> Maybe (TypeExp VName) -> SourcePos -> m ()
checkAnnot t mannot pos =
  case mannot of
    Nothing -> pure ()
    Just annot -> do
      case convertArrayTypeExp annot of
        Just at ->
          unless (t ~= at) $
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

withBind :: (MonadCheck m) => UniqueBind -> (Bind -> m a) -> m a
withBind (BindVal v mt ve pos) m = do
  ve' <- checkExp' ve
  let t = arrayTypeOf ve'
  mt' <- checkMaybeTypeExp mt
  checkAnnot t mt' pos
  withParam' (v, t) $ \vname ->
    m $ BindVal vname mt' ve' pos
withBind (BindFun f params mt body _ pos) m = do
  mt' <- checkMaybeTypeExp mt
  (params', body') <-
    binds (withPatParam checkTypeExp) params $ \params' -> do
      body' <- checkExp' body
      checkAnnot (arrayTypeOf body') mt' pos
      pure (params', body')
  let t = arrowType (map arrayTypeOf params') (arrayTypeOf body')
  withParam' (f, mkScalarArrayType t) $ \f' ->
    m $ BindFun f' params' mt' body' (Info t) pos
withBind (BindTFun f params mt body _ pos) m =
  binds withTypeParam params $ \params' -> do
    body' <- checkExp' body
    mt' <- checkMaybeTypeExp mt
    let body_t = arrayTypeOf body'
    checkAnnot body_t mt' pos
    let t = forallType params' body_t
    withParam' (f, mkScalarArrayType t) $ \f' ->
      m $ BindTFun f' params' mt' body' (Info t) pos
withBind (BindIFun f params mt body _ pos) m =
  binds withISpaceParam params $ \params' -> do
    body' <- checkExp' body
    mt' <- checkMaybeTypeExp mt
    let body_t = arrayTypeOf body'
    checkAnnot body_t mt' pos
    let t = piType params' body_t
    withParam' (f, mkScalarArrayType t) $ \f' ->
      m $ BindIFun f' params' mt' body' (Info t) pos
withBind (BindType tvar t _ pos) m =
  withType checkTypeExp (tvar, t) $ \(tvar', t') ->
    case convertTypeExp t' of
      Nothing -> throwErrorPos pos "Invalid type."
      Just t'' -> m $ BindType tvar' t' (Info t'') pos
withBind (BindISpace ivar ispace pos) m =
  withISpace checkISpace pos (ivar, ispace) $ \(ivar', ispace') ->
    m $ BindISpace ivar' ispace' pos

-- | Type check an unchecked 'Atom'.
checkAtom :: (MonadCheck m) => UniqueAtom -> m Atom
checkAtom (Base b _ pos) =
  pure $ Base b (Info $ baseTypeOf b) pos
checkAtom (Lambda param e _ pos) =
  withPatParam checkTypeExp param $ \param' -> do
    e' <- checkExp' e
    let r = arrayTypeOf e'
    pure $ Lambda param' e' (Info $ arrayTypeOf param' :-> r) pos
checkAtom (TLambda p e _ pos) =
  withTypeParam p $ \p' -> do
    e' <- checkExp' e
    let r = arrayTypeOf e'
    pure $ TLambda p' e' (Info $ Forall p' r) pos
checkAtom (ILambda p e _ pos) =
  withISpaceParam p $ \p' -> do
    e' <- checkExp' e
    let r = arrayTypeOf e'
    pure $ ILambda p' e' (Info $ Pi p' r) pos
checkAtom atom@(Box ispace e box_t _ pos) = do
  ispace' <- checkISpace ispace
  e' <- checkExp' e
  box_t' <- checkTypeExp box_t
  case convertAtomTypeExp box_t' of
    Just bt@(Sigma is t) -> do
      let t' = substitute (substISpaceVar (unISpaceParam is) ispace') t
      unless (arrayTypeOf e' ~= t') $
        throwErrorPos pos $
          T.unlines
            [ "Wrong box type.",
              "Expected:",
              prettyText $ typeOf e',
              "But got:",
              prettyText t'
            ]
      pure $ Box ispace' e' box_t' (Info bt) pos
    _ ->
      throwErrorPos pos $
        T.unlines
          [ "Non-existential box type:",
            prettyText atom
          ]

checkTypeExp :: (MonadCheck m) => TypeExp VName -> m (TypeExp VName)
checkTypeExp (TEAtomVar vname pos) = do
  inscopeAtomTypeVar vname
  mt <- lookupAtomType vname
  case mt of
    Nothing -> pure $ TEAtomVar vname pos
    Just t -> pure t
checkTypeExp te@(TEArrayVar _ _) =
  error $ "checkTypeExp: should never happen: " <> prettyString te
checkTypeExp (TEBool pos) = pure $ TEBool pos
checkTypeExp (TEInt pos) = pure $ TEInt pos
checkTypeExp (TEFloat pos) = pure $ TEFloat pos
checkTypeExp (TEArray t s pos) =
  TEArray <$> checkTypeExp t <*> checkShape s <*> pure pos
checkTypeExp (TEArrow t1 t2 pos) =
  TEArrow <$> checkTypeExp t1 <*> checkTypeExp t2 <*> pure pos
checkTypeExp (TEForall params t pos) =
  binds withTypeParamExp (NE.toList params) $ \params' ->
    TEForall (NE.fromList params') <$> checkTypeExp t <*> pure pos
checkTypeExp (TEPi params t pos) =
  binds withISpaceParam (NE.toList params) $ \params' ->
    TEPi (NE.fromList params') <$> checkTypeExp t <*> pure pos
checkTypeExp (TESigma params t pos) =
  binds withISpaceParam (NE.toList params) $ \params' ->
    TESigma (NE.fromList params') <$> checkTypeExp t <*> pure pos
