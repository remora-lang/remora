-- | This module implements *naive* lambda lifting. Faster algorithms are
-- available, see, e.g., Lambda-Lifting in Quadratic Time by Danvy and Schultz.
-- Also, computing the free variables in a separate walk is dumb, but probably
-- doesn't actually matter in practice.
module LambdaLift (lambdaLift, lambdaLiftExp) where

import Binds
import Control.Monad
import Control.Monad.Error.Class (liftEither)
import Control.Monad.State (state)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe
import Data.Set qualified as S
import Free
import LambdaLift.Monad
import Pass (PassM)
import Prop
import Syntax
import Util
import VName

lambdaLift :: Prog -> PassM Prog
lambdaLift p =
  liftEither =<< state (\tag -> runLift tag (liftProg p >>= insertLiftedBinds))
  where
    insertLiftedBinds p' =
      withLiftedBinds p' $ \bs -> Prog $ map Def (NE.toList bs) <> progDecs p'

lambdaLiftExp :: Exp -> PassM Exp
lambdaLiftExp e =
  liftEither =<< state (\tag -> runLift tag (liftExp e >>= insertLiftedBinds))
  where
    insertLiftedBinds e' =
      withLiftedBinds e' $ \bs -> Let bs e' (Info $ arrayTypeOf e') (posOf e')

liftProg :: Prog -> LiftM Prog
liftProg (Prog decs) = Prog <$> mapM liftDecl decs

withLiftedBinds :: a -> (NonEmpty Bind -> a) -> LiftM a
withLiftedBinds none some = do
  binds <- drainBinds
  pure $ maybe none some $ NE.nonEmpty binds

liftDecl :: Decl -> LiftM Decl
liftDecl (Def b) = Def <$> liftBind b
liftDecl (Entry f params mt body t pos) =
  Entry f params mt
    <$> bindTypes (paramTypes params) (liftExp body)
    <*> pure t
    <*> pure pos

liftBind :: Bind -> LiftM Bind
liftBind (BindVal v mt e pos) =
  BindVal v mt <$> liftExp e <*> pure pos
liftBind (BindFun f params mt body t pos) =
  BindFun f params mt
    <$> bindTypes (paramTypes $ NE.toList params) (liftExp body)
    <*> pure t
    <*> pure pos
liftBind b@BindType {} = pure b
liftBind b@BindISpace {} = pure b
liftBind BindTFun {} = error "liftBind: unsupported BindTFun"
liftBind BindIFun {} = error "liftBind: unsupported BindIFun"

paramTypes :: [Pat] -> [(VName, Syntax.ArrayType VName)]
paramTypes ps = map (\p -> (patVar p, arrayTypeOf p)) ps

liftExp :: Exp -> LiftM Exp
liftExp e@Var {} = pure e
liftExp e@EmptyArray {} = pure e
liftExp e@EmptyFrame {} = pure e
liftExp (Array shape atoms t pos)
  | isLambda $ NE.head atoms = do
      es <- mapM liftLambda atoms
      pure $ Frame shape es (Info $ arrayTypeOf $ NE.head es) pos
  | otherwise =
      Array shape <$> mapM liftAtom atoms <*> pure t <*> pure pos
  where
    liftLambda :: Atom -> LiftM Exp
    liftLambda (Lambda p body (Info lamt) _) =
      bindType (patVar p) (arrayTypeOf p) $ do
        free_vpts <- fmap
          catMaybes
          $ forM (S.toList $ patVar p `S.delete` freeVars body)
          $ \v -> (fmap . fmap) (v,) $ lookupType v
        v <- newVName "lambda"
        let ps' = NE.fromList $ (<> [p]) $ map mkParam $ free_vpts
            rt = findRet lamt
            t' = mkScalarArrayType $ fmap arrayTypeOf ps' `arrowType` rt
        emitBind =<< mkFunBind v ps' <$> liftExp body
        pure $ mkApp (Var v (Info t') noSrcPos) $ map mkArg free_vpts
      where
        mkParam (v, pt) = PatId v (ArrayType pt) (Info pt) noSrcPos
        mkArg (v, pt) = Var v (Info pt) noSrcPos
    liftLambda _ = error "liftLambda: not a lambda"

    isLambda :: Atom -> Bool
    isLambda Lambda {} = True
    isLambda _ = False
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
  Unbox ip x box' <$> bindType x xt (liftExp body) <*> pure t <*> pure pos
liftExp (Let binds body t pos) = do
  binds' <- liftBinds $ NE.toList binds
  body' <- withBinds binds' $ liftExp body
  pure $ Let (NE.fromList binds') body' t pos
  where
    liftBinds [] = pure []
    liftBinds (b : bs) = do
      b' <- liftBind b
      (b' :) <$> withBind b' (liftBinds bs)

liftAtom :: Atom -> LiftM Atom
liftAtom a@Base {} = pure a
liftAtom (Box i body te t pos) =
  Box i <$> liftExp body <*> pure te <*> pure t <*> pure pos
liftAtom Lambda {} = error "liftAtom: not allowed here buddy"
liftAtom TLambda {} = error "liftAtom: unsupported TLambda"
liftAtom ILambda {} = error "liftAtom: unsupported ILambda"
