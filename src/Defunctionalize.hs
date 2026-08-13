-- | A proto-defunctionalization pass.
--
-- Lambda lifting and monomorphization leave partial applications and function
-- values behind, which this pass tries to remove, since Futhark IR supports
-- neither partial application nor a variable holding a function.
--
-- A function binding whose body is a partial application is eta expanded:
--
-- > (def (add ((x Int)) (lam x)))
-- > (def (add ((x Int) (y Int)) (lam x y)))
--
-- A value binding whose value is a closure is substituted at its uses instead:
--
-- > (let ((f (lam g))) (f 2))
-- > (lam g 2)
--
-- This is why the pass is "proto-defunctionalization" (in the most generous
-- sense possible).
module Defunctionalize (defunctionalize, defunctionalizeExp) where

import Control.Monad (unless)
import Control.Monad.Reader
import Control.Monad.State
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Free (Free (..), FreeVars (..))
import Pass (PassM)
import Prop
import Syntax
import Util
import VName

newtype DefunM a = DefunM (ReaderT (Set VName) (StateT (Map VName Exp) PassM) a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader (Set VName),
      MonadState (Map VName Exp)
    )

instance MonadVName DefunM where
  getVarTag = DefunM $ lift $ lift getVarTag
  putVarTag = DefunM . lift . lift . putVarTag

runDefun :: DefunM a -> PassM a
runDefun (DefunM m) = evalStateT (runReaderT m mempty) mempty

bindLocals :: (Foldable t) => t VName -> DefunM a -> DefunM a
bindLocals vs = local $ \locals -> foldr S.insert locals vs

defunctionalize :: Prog -> PassM Prog
defunctionalize (Prog decs) = runDefun $ Prog . catMaybes <$> mapM defunDecl decs

defunctionalizeExp :: Exp -> PassM Exp
defunctionalizeExp = runDefun . defunExp

defunDecl :: Decl -> DefunM (Maybe Decl)
defunDecl (Def b) = fmap Def <$> defunBind b
defunDecl (Entry f ps mt body t pos) = do
  body' <- bindLocals (map patVar ps) $ defunExp body
  pure $ Just $ Entry f ps mt body' t pos

defunBind :: Bind -> DefunM (Maybe Bind)
defunBind b = do
  b' <- defunBindBody b
  case b' of
    BindVal v _ e _
      | isFunctionType $ arrayTypeOf e,
        isClosure e -> do
          modify $ M.insert v e
          pure Nothing
    BindTFun {} -> unsupported "type abstraction" b'
    BindIFun {} -> unsupported "index abstraction" b'
    _ -> Just <$> etaExpandBind b'
  where
    unsupported :: String -> Bind -> DefunM a
    unsupported what bind =
      error $
        unlines
          [ "defunBind: unsupported " <> what <> ":",
            prettyString bind
          ]

    isClosure :: Exp -> Bool
    isClosure Var {} = True
    isClosure (App f Var {} _ _) = isClosure f
    isClosure (TApp f _ _ _) = isClosure f
    isClosure (IApp f _ _ _) = isClosure f
    isClosure _ = False

defunBindBody :: Bind -> DefunM Bind
defunBindBody (BindVal v mt e pos) =
  BindVal v mt <$> defunExp e <*> pure pos
defunBindBody (BindFun f ps mt body t pos) =
  BindFun f ps mt
    <$> bindLocals (patVar <$> ps) (defunExp body)
    <*> pure t
    <*> pure pos
defunBindBody (BindTFun f ps mt body t pos) =
  BindTFun f ps mt <$> defunExp body <*> pure t <*> pure pos
defunBindBody (BindIFun f ps mt body t pos) =
  BindIFun f ps mt <$> defunExp body <*> pure t <*> pure pos
defunBindBody b@BindType {} = pure b
defunBindBody b@BindISpace {} = pure b

defunExp :: Exp -> DefunM Exp
defunExp e@(Var v _ _) = gets $ fromMaybe e . M.lookup v
defunExp e@EmptyArray {} = pure e
defunExp e@EmptyFrame {} = pure e
defunExp (Array s as t pos) =
  Array s <$> mapM defunAtom as <*> pure t <*> pure pos
defunExp (Frame s es t pos) =
  Frame s <$> mapM defunExp es <*> pure t <*> pure pos
defunExp (App f x t pos) =
  App <$> defunExp f <*> defunExp x <*> pure t <*> pure pos
defunExp (TApp e ta t pos) =
  TApp <$> defunExp e <*> pure ta <*> pure t <*> pure pos
defunExp (IApp e i t pos) =
  IApp <$> defunExp e <*> pure i <*> pure t <*> pure pos
defunExp (Unbox ip x box body t pos) =
  Unbox ip x
    <$> defunExp box
    <*> bindLocals [x] (defunExp body)
    <*> pure t
    <*> pure pos
defunExp (Let bs body t pos) =
  bindLocals (concatMap bindVars bs) $ do
    kept <- catMaybes <$> mapM defunBind (NE.toList bs)
    body' <- defunExp body
    pure $ case NE.nonEmpty kept of
      Nothing -> body'
      Just bs' -> Let bs' body' t pos
defunExp (Struct s t pos) = do
  let (fs, shps, es) = neUnzip3 s
  es' <- mapM defunExp es
  pure $ Struct (neZip3 fs shps es') t pos
defunExp (FieldProj e f t pos) =
  FieldProj <$> defunExp e <*> pure f <*> pure t <*> pure pos

defunAtom :: Atom -> DefunM Atom
defunAtom a@Base {} = pure a
defunAtom (Box i body te t pos) =
  Box i <$> defunExp body <*> pure te <*> pure t <*> pure pos
defunAtom a = error $ unlines ["defunAtom: unlifted lambda:", prettyString a]

etaExpandBind :: Bind -> DefunM Bind
etaExpandBind b@(BindVal v _ body _) = do
  ps <- etaParams body
  case NE.nonEmpty ps of
    Nothing
      | isFunctionType $ arrayTypeOf body ->
          error $ unlines ["etaExpandBind: cannot saturate:", prettyString b]
      | otherwise -> pure b
    Just ps' -> do
      captured <- asks $ S.intersection $ freeTermVars $ freeVars body
      unless (S.null captured) $
        error $
          unlines
            [ "etaExpandBind: " <> prettyString v <> " would capture:",
              prettyString (S.toList captured)
            ]
      pure $ mkFunBind v ps' $ etaApply body $ NE.toList ps'
etaExpandBind b@(BindFun v ps _ body _ _) = do
  ps' <- etaParams body
  pure $ case ps' of
    [] -> b
    _ -> mkFunBind v (NE.appendList ps ps') $ etaApply body ps'
etaExpandBind b = pure b

etaParams :: Exp -> DefunM [Pat]
etaParams body =
  mapM (\t -> (`mkParam` t) <$> newVName "x") $
    fst $
      unfoldArrow $
        arrayTypeOf body

etaApply :: Exp -> [Pat] -> Exp
etaApply body ps = apply body $ map (uncurry mkVar) $ patVarTypes ps
  where
    apply e [] = e
    apply (Let binds e _ pos) args = rewrap (Let binds) pos $ apply e args
    apply (Unbox ip x box e _ pos) args = rewrap (Unbox ip x box) pos $ apply e args
    apply e args = mkApp e args

    rewrap wrap pos e = wrap e (Info $ arrayTypeOf e) pos
