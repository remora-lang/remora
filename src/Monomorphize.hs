module Monomorphize (monomorphize, monomorphizeExp) where

import Binds (collectBinds, emitBind)
import Control.Monad.Error.Class
import Control.Monad.State
import Data.Bifunctor
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe
import Intrinsics (isIntrinsic)
import Monomorphize.Monad
import Pass (PassM)
import Prop
import Rename (renameBind, renameExp)
import Substitute
import Syntax hiding
  ( ArrayType,
    AtomType,
    ISpace,
    ISpaceParam,
    bindVars,
  )
import Util
import VName

monomorphize :: Prog -> PassM Prog
monomorphize p =
  liftEither =<< state (\tag -> runMono tag (monoProg p >>= insertBinds))

monomorphizeExp :: Exp -> PassM Exp
monomorphizeExp e =
  liftEither =<< state (\tag -> runMono tag (monoExp e >>= insertBindsExp))

monoProg :: Prog -> MonoM Prog
monoProg (Prog decs) = Prog <$> monoDecls decs

-- | A top-level bind is in scope for every declaration that follows it.
monoDecls :: [Decl] -> MonoM [Decl]
monoDecls [] = pure []
monoDecls (Def b : decs) =
  withBind b $ \instances kept ->
    ((map Def instances <> maybe [] (pure . Def) kept) <>) <$> monoDecls decs
monoDecls (Entry f ps mt body t pos : decs) = do
  (entry, instances) <-
    collectBinds $
      bindVars (patVarTypes ps) $
        Entry f ps mt <$> monoExp body <*> pure t <*> pure pos
  ((map Def instances <> [entry]) <>) <$> monoDecls decs

monoAtom :: Atom -> MonoM Atom
monoAtom a@Base {} = pure a
monoAtom (Lambda p body t pos) =
  bindVar (patVar p) (arrayTypeOf p) $
    Lambda p <$> monoExp body <*> pure t <*> pure pos
monoAtom (TLambda p body t pos) =
  TLambda p <$> monoExp body <*> pure t <*> pure pos
monoAtom (ILambda p body t pos) =
  bindISpaceParams [p] $
    ILambda p <$> monoExp body <*> pure t <*> pure pos
monoAtom (Box i body te t pos) =
  Box i <$> monoExp body <*> pure te <*> pure t <*> pure pos

monoExp :: Exp -> MonoM Exp
monoExp e@Var {} = pure e
monoExp e@EmptyArray {} = pure e
monoExp e@EmptyFrame {} = pure e
monoExp (Array s as t pos) =
  Array s <$> mapM monoAtom as <*> pure t <*> pure pos
monoExp (Frame s es t pos) =
  Frame s <$> mapM monoExp es <*> pure t <*> pure pos
monoExp (App f arg t pos) =
  App <$> monoExp f <*> monoExp arg <*> pure t <*> pure pos
monoExp e@TApp {} = monoPolyApp e
monoExp e@IApp {} = monoPolyApp e
monoExp (Unbox p x box body t pos) = do
  box' <- monoExp box
  let xt =
        fromMaybe (error "monoExp: unbox of non-existential") $
          unboxType p $
            arrayTypeOf box'
  bindISpaceParams [p] $
    bindVar x xt $
      Unbox p x box' <$> monoExp body <*> pure t <*> pure pos
monoExp (Let bs body t pos) =
  withBinds (NE.toList bs) $ \kept -> do
    body' <- monoExp body
    pure $
      case NE.nonEmpty kept of
        Nothing -> body'
        Just bs' -> Let bs' body' t pos
monoExp (Struct s t pos) = do
  let (fs, shps, es) = neUnzip3 s
  es' <- mapM monoExp es
  let s' = neZip3 fs shps es'
  pure $ Struct s' t pos
monoExp (FieldProj e f t pos) =
  FieldProj <$> monoExp e <*> pure f <*> pure t <*> pure pos

withBinds :: [Bind] -> ([Bind] -> MonoM a) -> MonoM a
withBinds [] k = k []
withBinds (b : bs) k =
  withBind b $ \instances mkept -> do
    mapM_ emitBind instances
    maybe id bindLocal mkept $ withBinds bs $ k . maybe id (:) mkept
  where
    bindLocal :: Bind -> MonoM a -> MonoM a
    bindLocal kept m =
      case bindName kept of
        Just v -> bindVar v (arrayTypeOf kept) m
        Nothing -> m

withBind :: Bind -> ([Bind] -> Maybe Bind -> MonoM a) -> MonoM a
withBind (BindTFun v ps _ body _ _) m =
  withPolyDef v (ParamType <$> NE.toList ps) body m
withBind (BindIFun v ps _ body _ _) m =
  withPolyDef v (ParamISpace <$> NE.toList ps) body m
withBind (BindVal v mt e pos) m = do
  poly <- asPoly e
  case poly of
    Just poly' -> bindDef v poly' $ m mempty Nothing
    Nothing -> do
      (e', instances) <- collectBinds $ monoExp e
      m instances $ Just $ BindVal v mt e' pos
withBind (BindFun f ps mt body t pos) m = do
  (body', instances) <-
    collectBinds $ bindVars (patVarTypes $ NE.toList ps) $ monoExp body
  m instances $ Just $ BindFun f ps mt body' t pos
withBind b@(BindISpace ip _ _) m = bindISpaceParams [ip] $ m mempty $ Just b
withBind b@BindType {} m = m mempty $ Just b

withPolyDef :: VName -> [Param] -> Exp -> ([Bind] -> Maybe Bind -> MonoM a) -> MonoM a
withPolyDef v ps body k =
  bindDef v (PolyFun (Just v) (ps <> ps') freeBody) $ k mempty Nothing
  where
    (ps', freeBody) = unfoldLambda body

monoPolyApp :: Exp -> MonoM Exp
monoPolyApp e = do
  poly <- asPoly f
  case poly of
    Nothing -> unresolved
    Just poly' -> do
      cached <- lookupCached
      case cached of
        Just e' -> pure e'
        Nothing -> do
          result <- instantiate =<< specialize poly' args
          case f of
            Var v _ _ -> emitMonoExp (v, args) result
            _ -> pure ()
          pure result
  where
    (f, args) = unfoldArgs e

    unresolved =
      case f of
        -- This should always be an intrinsic
        Var v _ pos
          | isIntrinsic v ->
              pure $ Var v (Info $ arrayTypeOf e) pos
        _ ->
          throwError $
            "monoPolyApp: unresolved polymorphic value applied to "
              <> prettyText (length args)
              <> " argument(s):\n"
              <> prettyText e

    lookupCached =
      case f of
        Var v _ _ -> lookupMono v args
        _ -> pure Nothing

specialize :: Poly -> [Arg] -> MonoM Poly
specialize poly [] = pure poly
specialize (PolyFun mv (p : ps) body) (arg : args) =
  specialize (PolyFun mv ps $ substitute (argSubst p arg) body) args
specialize (PolyFun mv [] body) args = do
  poly <- asPoly f
  case poly of
    Just poly' -> specialize poly' $ bodyArgs <> args
    Nothing
      | Var v _ _ <- f,
        isIntrinsic v ->
          pure $ PolyFun mv [] $ foldl applyArg body args
      | otherwise ->
          throwError $
            "specialize: unresolved polymorphic value:\n" <> prettyText body
  where
    (f, bodyArgs) = unfoldArgs body
specialize (PolyArray s ps) args =
  PolyArray s <$> mapM (`specialize` args) ps

applyArg :: Exp -> Arg -> Exp
applyArg e (ArgType at) = applyTypeArg e at
applyArg e (ArgISpace isp) = applyISpaceArg e isp

argSubst :: Param -> Arg -> Subst VName
argSubst (ParamType tp) (ArgType at) = substAtomVar (unTypeParam tp) at
argSubst (ParamISpace ip) (ArgISpace isp) = substISpaceVar (unISpaceParam ip) isp
argSubst _ _ = error "argSubst: parameter/argument mismatch"

instantiate :: Poly -> MonoM Exp
instantiate (PolyFun mv [] body) = do
  v <- maybe (newVName "mono") (newVName . (<> "_mono") . varName) mv
  body' <- monoExp =<< renameExp body
  (values, ispaces) <- captured body'
  bind <-
    renameBind $
      addISpaceParams ispaces $
        case NE.nonEmpty $ map (uncurry mkParam) values of
          Nothing -> BindVal v Nothing body' noSrcPos
          Just ps -> mkFunBind v ps body'
  emitBind bind
  pure $
    mkApp
      (foldl mkISpaceApp (mkVar (bindNameOf bind) $ arrayTypeOf bind) ispaces)
      (map (uncurry mkVar) values)
instantiate (PolyArray s ps) = do
  es <- mapM instantiate ps
  pure $ frameOf (posOf $ NE.head es) s es
instantiate PolyFun {} =
  throwError "instantiate: unsupported: partially applied poly value"

bindParams :: Bind -> Maybe ([Param], Exp)
bindParams (BindTFun _ ps _ body _ _) = Just (ParamType <$> NE.toList ps, body)
bindParams (BindIFun _ ps _ body _ _) = Just (ParamISpace <$> NE.toList ps, body)
bindParams _ = Nothing

unfoldLambda :: Exp -> ([Param], Exp)
unfoldLambda e
  | Just (TLambda p inner _ _) <- asScalar e =
      first (ParamType p :) $ unfoldLambda inner
  | Just (ILambda p inner _ _) <- asScalar e =
      first (ParamISpace p :) $ unfoldLambda inner
  | Let (b :| []) (Var f _ _) _ _ <- e,
    bindName b == Just f,
    Just (ps, body) <- bindParams b =
      first (ps <>) $ unfoldLambda body
  | otherwise = ([], e)

asPoly :: Exp -> MonoM (Maybe Poly)
asPoly (Var v _ _) = lookupDef v
asPoly (Array s as (Info (et :@ _)) _)
  | isPolymorphic et =
      pure $ Just $ PolyArray s (atomToPoly <$> as)
asPoly (Frame s es (Info (et :@ _)) _)
  | isPolymorphic et = do
      polys <- mapM asPoly es
      pure $ PolyArray s <$> sequence polys
asPoly _ = pure Nothing

unfoldArgs :: Exp -> (Exp, [Arg])
unfoldArgs = second reverse . unfoldArgs'
  where
    unfoldArgs' (TApp tf te _ _) =
      case fromAtomType te of
        Just at -> second (ArgType at :) $ unfoldArgs' tf
        Nothing -> error "unfoldArgs: not atom type"
    unfoldArgs' (IApp f is _ _) =
      second (ArgISpace is :) $ unfoldArgs' f
    unfoldArgs' e = (e, mempty)

atomToPoly :: Atom -> Poly
atomToPoly (TLambda p body _ _) =
  PolyFun Nothing (ParamType p : ps) freeBody
  where
    (ps, freeBody) = unfoldLambda body
atomToPoly (ILambda p body _ _) =
  PolyFun Nothing (ParamISpace p : ps) freeBody
  where
    (ps, freeBody) = unfoldLambda body
atomToPoly _ = error "atomToPoly"

isPolymorphic :: AtomType -> Bool
isPolymorphic Forall {} = True
isPolymorphic Pi {} = True
isPolymorphic _ = False
