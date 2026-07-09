module Monomorphize (monomorphize, monomorphizeExp) where

import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.State
import Data.Bifunctor
import Data.Either
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe
import Monomorphize.Monad
import Pass (PassM)
import Prop
import Substitute
import Syntax hiding (ArrayType, AtomType, ISpace, Type, TypeExp)
import Util
import VName

monomorphize :: Prog -> PassM Prog
monomorphize p =
  liftEither
    =<< state (\tag -> runMono tag (monoProg p >>= insertMonoBinds))
  where
    insertMonoBinds =
      flip withMonoBinds $ \bs -> Prog $ map Def (NE.toList bs) <> progDecs p

monomorphizeExp :: Exp -> PassM Exp
monomorphizeExp e =
  liftEither
    =<< state (\tag -> runMono tag (monoExp e >>= insertMonoBinds))
  where
    insertMonoBinds =
      flip withMonoBinds $ \bs -> Let bs e (Info $ arrayTypeOf e) (posOf e)

withMonoBinds :: a -> (NonEmpty Bind -> a) -> MonoM a
withMonoBinds none some = do
  binds <- gets stateMonoBinds
  pure $ maybe none some $ NE.nonEmpty $ reverse binds

monoProg :: Prog -> MonoM Prog
monoProg (Prog decs) = Prog . catMaybes <$> mapM monoDecl decs

monoDecl :: Decl -> MonoM (Maybe Decl)
monoDecl (Def b) = do
  mb <- addBind b
  traverse (fmap Def . monoBind) mb
monoDecl (Entry f ps mt body t pos) = do
  body' <- monoExp body
  pure $ Just $ Entry f ps mt body' t pos

monoAtom :: Atom -> MonoM Atom
monoAtom a@Base {} = pure a
monoAtom (Lambda p body t pos) =
  Lambda p <$> monoExp body <*> pure t <*> pure pos
monoAtom (TLambda p body t pos) =
  TLambda p <$> monoExp body <*> pure t <*> pure pos
monoAtom (ILambda p body t pos) =
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
monoExp (Unbox p x box body t pos) =
  Unbox p x <$> monoExp box <*> monoExp body <*> pure t <*> pure pos
monoExp (Let bs body t pos) = do
  keep <- catMaybes . NE.toList <$> mapM addBind bs
  binds <- mapM monoBind keep
  body' <- monoExp body
  pure $
    case NE.nonEmpty binds of
      Nothing -> body'
      Just bs' -> Let bs' body' t pos

monoBind :: Bind -> MonoM Bind
monoBind (BindVal x mt e pos) =
  BindVal x mt <$> monoExp e <*> pure pos
monoBind (BindFun f ps mt body t pos) =
  BindFun f ps mt <$> monoExp body <*> pure t <*> pure pos
monoBind b@BindType {} = pure b
monoBind b@BindISpace {} = pure b
monoBind BindTFun {} = error "monoBind: impossible (hopefully)"
monoBind BindIFun {} = error "monoBind: impossible (hopefully)"

monoPolyApp :: Exp -> MonoM Exp
monoPolyApp e = resolveApp e >>= checkMono
  where
    checkMono Left {} =
      throwError "monomorphize: unsupported: partially applied polymorphic value"
    checkMono (Right e') = pure e'

addBind :: Bind -> MonoM (Maybe Bind)
addBind (BindTFun v _ _ body (Info t) _) = do
  insertDef v (PolyFun (Just v) body t)
  pure Nothing
addBind (BindIFun v _ _ body (Info t) _) = do
  insertDef v (PolyFun (Just v) body t)
  pure Nothing
addBind b@(BindVal v _ e _)
  | Just poly <- asPoly e = do
      insertDef v poly
      pure Nothing
  | otherwise = pure $ Just b
addBind b = pure $ Just b

asPoly :: Exp -> Maybe Poly
asPoly (Array s as (Info (et :@ _)) _)
  | isPolymorphic et =
      Just $ PolyArray s (atomToPoly <$> as)
asPoly _ = Nothing

-- Type arguments have their original source type expressions bundled with
-- them so that we can substitute into them post monomorphization so we don't
-- get gross AST nodes with out-of-sync type expressions.
type Arg = Either (AtomType, TypeExp) ISpace

argToMonoArg :: Arg -> MonoArg
argToMonoArg (Left (at, _)) = Left at
argToMonoArg (Right is) = Right is

unfoldApp :: Exp -> (Exp, [Arg])
unfoldApp = second reverse . unfoldApp'
  where
    unfoldApp' (TApp tf te _ _) =
      case convertAtomTypeExp te of
        Just at -> second (Left (at, te) :) $ unfoldApp' tf
        Nothing -> error "unfoldApp: not atom type"
    unfoldApp' (IApp f is _ _) =
      second (Right is :) $ unfoldApp' f
    unfoldApp' e = (e, mempty)

resolveApp :: Exp -> MonoM (Either Poly Exp)
resolveApp e = do
  resolved <- resolveFun f
  case resolved of
    Right e' ->
      pure $
        Right $
          case e' of
            -- This should always be an intrinsic
            Var v _ pos ->
              Var v (Info $ arrayTypeOf e) pos
            _ -> e'
    Left poly -> do
      cached <- lookupCached
      case cached of
        Just v' -> pure $ Right $ Var v' (Info $ arrayTypeOf e) noSrcPos
        Nothing -> do
          result <- foldM step (Left poly) args
          case (f, result) of
            (Var v _ _, Right (Var v' _ _)) -> do
              emitMonoVName (v, marg) v'
              pure result
            _ -> pure result
  where
    (f, args) = unfoldApp e
    marg = map argToMonoArg args

    resolveFun fn@(Var v _ _) =
      maybe (Right fn) Left <$> lookupDef v
    resolveFun fn =
      maybe (Right <$> monoExp fn) (pure . Left) $ asPoly fn

    lookupCached =
      case f of
        Var v _ _ -> lookupMono v marg
        _ -> pure Nothing

    step (Left poly) arg = specialize poly arg
    step (Right _) _ = error "resolveApp: over-applied"

specialize :: Poly -> Arg -> MonoM (Either Poly Exp)
specialize (PolyFun mv body t) arg
  | isPolymorphic et && isScalar s =
      pure $ Left $ PolyFun mv body' et
  | otherwise = do
      v <- maybe (newVName "mono") (newVName . (<> "_mono") . varName) mv
      body'' <- monoExp body'
      emitMonoBind $ BindVal v Nothing body'' noSrcPos
      pure $ Right $ Var v (Info $ arrayTypeOf body'') noSrcPos
  where
    (subst, rt) =
      case (t, arg) of
        (Forall tp r, Left (at, te)) ->
          ( substAtomVar (unTypeParam tp) at
              <> substTypeExpVar (unTypeParam tp) te,
            r
          )
        (Pi ip r, Right is) -> (substISpaceVar (unISpaceParam ip) is, r)
        _ -> error "specialize: abstraction/argument kind mismatch"
    et :@ s = substitute subst rt
    body' = substitute subst body
specialize (PolyArray s ps) arg = do
  results <- mapM (`specialize` arg) ps
  pure $
    case partitionEithers $ NE.toList results of
      ([], es) -> Right $ frame $ NE.fromList es
      (p : ps', []) -> Left $ PolyArray s (p :| ps')
      _ -> error "specialize: mix of argument types"
  where
    frame es@(e :| _) =
      let et :@ sh = arrayTypeOf e
       in flattenExp $ Frame s es (Info $ et :@ (intsToShape s <> sh)) (posOf e)

atomToPoly :: Atom -> Poly
atomToPoly (TLambda _ body (Info t) _) = PolyFun Nothing body t
atomToPoly (ILambda _ body (Info t) _) = PolyFun Nothing body t
atomToPoly _ = error "atomToPoly"

isPolymorphic :: AtomType -> Bool
isPolymorphic Forall {} = True
isPolymorphic Pi {} = True
isPolymorphic _ = False
