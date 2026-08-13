-- | This module implements *naive* lambda lifting. Faster algorithms are
-- available, see, e.g., Lambda-Lifting in Quadratic Time by Danvy and Schultz.
-- Also, computing the free variables in a separate walk is dumb, but probably
-- doesn't actually matter in practice.
module LambdaLift (lambdaLift, lambdaLiftExp) where

import Binds (collectBinds, emitBind)
import Control.Monad (forM)
import Control.Monad.State (state)
import Data.List (partition)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe
import LambdaLift.Monad
import Pass (PassM)
import Prop
import Rename (renameBind)
import Syntax hiding (ArrayType, ISpaceParam, TypeParam, bindVars)
import Util
import VName

lambdaLift :: Prog -> PassM Prog
lambdaLift p =
  state $ \tag -> runLift tag $ liftProg p >>= insertBinds

lambdaLiftExp :: Exp -> PassM Exp
lambdaLiftExp e =
  state $ \tag -> runLift tag $ liftExp e >>= insertBindsExp

liftProg :: Prog -> LiftM Prog
liftProg (Prog decs) = fmap (Prog . concat) $ forM decs $ \dec -> do
  (dec', lifted) <- collectBinds $ liftDecl dec
  pure $ map Def lifted <> [dec']

liftDecl :: Decl -> LiftM Decl
liftDecl (Def b) = Def <$> liftBind b
liftDecl (Entry f params mt body t pos) =
  Entry f params mt
    <$> bindVars (patVarTypes params) (liftExp body)
    <*> pure t
    <*> pure pos

liftBind :: Bind -> LiftM Bind
liftBind (BindVal v mt e pos) =
  BindVal v mt <$> liftExp e <*> pure pos
liftBind (BindFun f params mt body t pos) =
  BindFun f params mt
    <$> bindVars (patVarTypes $ NE.toList params) (liftExp body)
    <*> pure t
    <*> pure pos
liftBind (BindTFun f params mt body t pos) =
  BindTFun f params mt
    <$> bindTypeParams params (liftExp body)
    <*> pure t
    <*> pure pos
liftBind (BindIFun f params mt body t pos) =
  BindIFun f params mt
    <$> bindISpaceParams params (liftExp body)
    <*> pure t
    <*> pure pos
liftBind b@BindType {} = pure b
liftBind b@BindISpace {} = pure b

liftExp :: Exp -> LiftM Exp
liftExp e@(Var v _ _) = fromMaybe e <$> lookupLifted v
liftExp e@EmptyArray {} = pure e
liftExp e@EmptyFrame {} = pure e
liftExp (Array [] (atom :| []) _ _)
  | isLambda atom =
      liftLambda atom
liftExp (Array shape atoms t pos)
  | all isLambda atoms = do
      es <- mapM liftLambda atoms
      pure $ Frame shape es (Info $ arrayTypeOf $ NE.head es) pos
  | otherwise =
      Array shape <$> mapM liftAtom atoms <*> pure t <*> pure pos
liftExp (Frame shape es t pos) =
  Frame shape <$> mapM liftExp es <*> pure t <*> pure pos
liftExp (App f x t pos) =
  App <$> liftExp f <*> liftExp x <*> pure t <*> pure pos
liftExp (TApp e ta t pos) =
  TApp <$> liftExp e <*> pure ta <*> pure t <*> pure pos
liftExp (IApp e i t pos) =
  IApp <$> liftExp e <*> pure i <*> pure t <*> pure pos
liftExp (Unbox ip x box body t pos) = do
  box' <- liftExp box
  let xt =
        fromMaybe (error "liftExp: unbox of non-existential") $
          unboxType ip $
            arrayTypeOf box'
  Unbox ip x box'
    <$> bindISpaceParams [ip] (bindVar x xt (liftExp body))
    <*> pure t
    <*> pure pos
liftExp (Let binds body t pos) = do
  binds' <- liftBinds $ NE.toList binds
  body' <- withBinds binds' $ liftExp body
  pure $ case NE.nonEmpty binds' of
    Nothing -> body'
    Just bs' -> Let bs' body' t pos
  where
    liftBinds [] = pure []
    liftBinds (b : bs) = do
      kept <- liftLocalFun =<< liftBind b
      case kept of
        Nothing -> liftBinds bs
        Just b' -> (b' :) <$> withBind b' (liftBinds bs)
liftExp (Struct s t pos) = do
  let (fs, shps, es) = neUnzip3 s
  es' <- mapM liftExp es
  let s' = neZip3 fs shps es'
  pure $ Struct s' t pos
liftExp (FieldProj e f t pos) =
  FieldProj <$> liftExp e <*> pure f <*> pure t <*> pure pos

liftAtom :: Atom -> LiftM Atom
liftAtom a@Base {} = pure a
liftAtom (Box i body te t pos) =
  Box i <$> liftExp body <*> pure te <*> pure t <*> pure pos
liftAtom a@Lambda {} =
  error $ unlines ["liftAtom: unlifted lambda:", prettyString a]
liftAtom a@TLambda {} =
  error $ unlines ["liftAtom: unlifted lambda:", prettyString a]
liftAtom a@ILambda {} =
  error $ unlines ["liftAtom: unlifted lambda:", prettyString a]

liftLocalFun :: Bind -> LiftM (Maybe Bind)
liftLocalFun b@(BindFun f params mt body t pos) = do
  capt <- captured b
  case partition (isFunctionType . snd) $ capturedTerms capt of
    ([], _) -> do
      f' <- newVName $ varName f
      addLifted f =<< hoistBind (BindFun f' params mt body t pos)
      pure Nothing
    (_, []) -> pure $ Just b
    (funs, vals) ->
      error $
        unlines
          [ "liftLocalFun: cannot lift " <> prettyString f <> ":",
            "captures functions " <> prettyString (map fst funs),
            "and values " <> prettyString (map fst vals),
            prettyString b
          ]
liftLocalFun b = pure $ Just b

liftLambda :: Atom -> LiftM Exp
liftLambda lam = hoistBind =<< mkLambdaBindM lam

hoistBind :: Bind -> LiftM Exp
hoistBind inner = do
  capt <- captured inner
  bind <-
    renameBind $
      addTypeParams (capturedTypes capt) $
        addISpaceParams (capturedISpaces capt) $
          addTermParams (capturedParams capt) inner
  emitBind bind
  pure $ appCaptured (bindNameOf bind) capt $ arrayTypeOf bind

mkLambdaBindM :: Atom -> LiftM Bind
mkLambdaBindM (Lambda p body _ _) = do
  v <- newVName "lam"
  uncurry (mkFunBind v) <$> flattenLambdas p body
  where
    flattenLambdas param e =
      bindVar (patVar param) (arrayTypeOf param) $
        case asScalar e of
          Just (Lambda param' inner _ _) -> do
            (ps, body') <- flattenLambdas param' inner
            pure (param NE.<| ps, body')
          _ -> do
            body' <- liftExp e
            pure (param :| [], body')
mkLambdaBindM (TLambda tp body _ _) = do
  v <- newVName "tlam"
  body' <- bindTypeParams [tp] $ liftExp body
  pure $ BindTFun v (tp :| []) Nothing body' (Info $ Forall tp $ arrayTypeOf body') noSrcPos
mkLambdaBindM (ILambda ip body _ _) = do
  v <- newVName "ilam"
  body' <- bindISpaceParams [ip] $ liftExp body
  pure $ BindIFun v (ip :| []) Nothing body' (Info $ Pi ip $ arrayTypeOf body') noSrcPos
mkLambdaBindM atom =
  error $ unlines ["mkLambdaBindM: not a lambda:", prettyString atom]
