module Monomorphize (monomorphize, monomorphizeExp) where

import Binds (drainBinds, emitBind)
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
    insertMonoBinds p' =
      withMonoBinds p' $ \bs -> Prog $ map Def (NE.toList bs) <> progDecs p'

monomorphizeExp :: Exp -> PassM Exp
monomorphizeExp e =
  liftEither
    =<< state (\tag -> runMono tag (monoExp e >>= insertMonoBinds))
  where
    insertMonoBinds e' =
      withMonoBinds e' $ \bs -> Let bs e' (Info $ arrayTypeOf e') (posOf e')

withMonoBinds :: a -> (NonEmpty Bind -> a) -> MonoM a
withMonoBinds none some = do
  binds <- drainBinds
  pure $ maybe none some $ NE.nonEmpty binds

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
addBind b
  | Just v <- bindName b,
    Just (ps, body) <- bindParams b = do
      let (ps', freeBody) = unfoldAbs body
      insertDef v (PolyFun (Just v) (ps <> ps') freeBody)
      pure Nothing
addBind (BindVal v _ e _)
  | Just poly <- asPoly e = do
      insertDef v poly
      pure Nothing
addBind b = pure $ Just b

bindParams :: Bind -> Maybe ([Param], Exp)
bindParams (BindTFun _ ps _ body _ _) = Just (Left <$> NE.toList ps, body)
bindParams (BindIFun _ ps _ body _ _) = Just (Right <$> NE.toList ps, body)
bindParams _ = Nothing

unfoldAbs :: Exp -> ([Param], Exp)
unfoldAbs e
  | Just (TLambda p inner _ _) <- asScalar e =
      first (Left p :) $ unfoldAbs inner
  | Just (ILambda p inner _ _) <- asScalar e =
      first (Right p :) $ unfoldAbs inner
  | Let (b :| []) (Var f _ _) _ _ <- e,
    bindName b == Just f,
    Just (ps, body) <- bindParams b =
      first (ps <>) $ unfoldAbs body
  | otherwise = ([], e)

asPoly :: Exp -> Maybe Poly
asPoly (Array s as (Info (et :@ _)) _)
  | isPolymorphic et =
      Just $ PolyArray s (atomToPoly <$> as)
asPoly _ = Nothing

argToMonoArg :: Arg -> MonoArg
argToMonoArg (Left at) = Left at
argToMonoArg (Right is) = Right is

unfoldApp :: Exp -> (Exp, [Arg])
unfoldApp = second reverse . unfoldApp'
  where
    unfoldApp' (TApp tf te _ _) =
      case fromAtomType te of
        Just at -> second (Left at :) $ unfoldApp' tf
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
specialize (PolyFun mv (p : ps) body) arg
  | null ps = do
      v <- maybe (newVName "mono") (newVName . (<> "_mono") . varName) mv
      body'' <- monoExp body'
      emitBind $ BindVal v Nothing body'' noSrcPos
      pure $ Right $ Var v (Info $ arrayTypeOf body'') noSrcPos
  | otherwise = pure $ Left $ PolyFun mv ps body'
  where
    subst =
      case (p, arg) of
        (Left tp, Left at) -> substAtomVar (unTypeParam tp) at
        (Right ip, Right is) -> substISpaceVar (unISpaceParam ip) is
        _ -> error "specialize: abstraction/argument kind mismatch"
    body' = substitute subst body
specialize (PolyFun _ [] _) _ = error "specialize: over-applied"
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
atomToPoly (TLambda p body _ _) =
  PolyFun Nothing (Left p : ps) freeBody
  where
    (ps, freeBody) = unfoldAbs body
atomToPoly (ILambda p body _ _) =
  PolyFun Nothing (Right p : ps) freeBody
  where
    (ps, freeBody) = unfoldAbs body
atomToPoly _ = error "atomToPoly"

isPolymorphic :: AtomType -> Bool
isPolymorphic Forall {} = True
isPolymorphic Pi {} = True
isPolymorphic _ = False
