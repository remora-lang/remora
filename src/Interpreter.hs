module Interpreter where

import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.Reader hiding (lift)
import Control.Monad.Trans.Except
import Data.List qualified as L
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Interpreter.Value hiding (Val)
import Interpreter.Value qualified as Value
import Prop hiding ((\\))
import RemoraPrelude (Prelude, PreludeVal (..))
import Syntax hiding (Atom, Bind, Dim, Exp, Extent, Shape, Type)
import Syntax qualified
import Util
import VName

type Dim = Syntax.Dim VName

type Val = Value.Val InterpM

type Exp = Syntax.Exp Info VName

type Atom = Syntax.Atom Info VName

type Shape = Syntax.Shape VName

type Type = Syntax.Type Info VName

type Extent = Syntax.Extent VName

type Bind = Syntax.Bind Info VName

-- | Interpret a program. Takes a type checked prelude to populate the initial
-- environment with.
interpret :: Prelude Info VName InterpM -> Exp -> Either Error Val
interpret prelude e =
  runReader (runExceptT $ runInterpM $ intExp e) (initEnv prelude)

-- | The interpreter environment.
data Env = Env
  { -- | Variable to values.
    envMap :: Map VName Val,
    -- | Type variables to types.
    envTMap :: Map VName Type,
    -- | Dim variables to dim literals.
    envDMap :: Map VName Int,
    -- | Shape variables to shape literals.
    envSMap :: Map VName [Int]
  }

-- | The initial environment. Takes a type checked prelude to populate the
-- initial variable and type mappings.
initEnv :: Prelude Info VName InterpM -> Env
initEnv prelude = Env m tm mempty mempty
  where
    m =
      M.fromList $
        map (\(PreludeVal v _ val) -> (v, val)) prelude
    tm =
      M.fromList $
        map (\(PreludeVal v t _) -> (v, ArrayType t)) prelude

type Error = Text

-- | The interpreter monad.
newtype InterpM a = InterpM {runInterpM :: ExceptT Error (Reader Env) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader Env,
      MonadError Error
    )

-- | Lookup a variable's value.
lookupVal :: VName -> InterpM Val
lookupVal v = do
  mval <- asks ((M.!? v) . envMap)
  case mval of
    Nothing -> do
      m <- asks envMap
      error $
        unlines
          [ "lookupVal: unknown vname",
            prettyString v,
            prettyString $ M.keys m
          ]
    Just val -> pure val

-- | Lookup a shape variable.
lookupShape :: VName -> InterpM [Int]
lookupShape v = do
  ms <- asks ((M.!? v) . envSMap)
  case ms of
    Nothing ->
      error $
        unlines
          [ "lookupShape: unknown vname",
            prettyString v
          ]
    Just s -> pure s

---- | Lookup a dim variable.
lookupDim :: VName -> InterpM Int
lookupDim v = do
  md <- asks ((M.!? v) . envDMap)
  case md of
    Nothing ->
      error $
        unlines
          [ "lookupDim: unknown vname",
            prettyString v
          ]
    Just d -> pure d

-- | Locally bind a variable to a value.
bind :: VName -> Val -> InterpM a -> InterpM a
bind v val =
  local (\env -> env {envMap = M.insert v val $ envMap env})

-- | Locally bind a type variable to a type.
tbind :: VName -> Type -> InterpM a -> InterpM a
tbind v t =
  local (\env -> env {envTMap = M.insert v t $ envTMap env})

-- | Locally bind an index variable to a shape literal.
ibind :: VName -> Either Int [Int] -> InterpM a -> InterpM a
ibind v (Left d) =
  local (\env -> env {envDMap = M.insert v d $ envDMap env})
ibind v (Right s) =
  local (\env -> env {envSMap = M.insert v s $ envSMap env})

-- | Many binds.
binds :: (v -> val -> a -> a) -> [(v, val)] -> a -> a
binds _ [] m = m
binds f ((v, val) : vvals) m =
  f v val $ binds f vvals m

-- | Intepret an expression.
intExp :: Exp -> InterpM Val
intExp (Var v _ _) = lookupVal v
intExp (Array shape as _ _) =
  ValArray shape <$> mapM intAtom as
intExp (EmptyArray shape _ _ _) =
  pure $ ValArray shape mempty
intExp (Frame shape es _ _) =
  ValArray shape <$> mapM intExp es
intExp (EmptyFrame shape _ _ _) =
  pure $ ValArray shape mempty
intExp expr@(App f es (Info (_, pframe)) _) = do
  pframe' <- intShape pframe
  f' <- arrayifyVal <$> intExp f
  argvs <- mapM intExp es
  arrayTypeView (typeOf f) $ \(t, _) ->
    case t of
      pts :-> _ -> do
        sparams <- mapM (intShape . shapeOf) pts
        let (f'', argvs') = lift pframe' f' (sparams, argvs)
        apMap f'' (sparams, argvs')
      _ ->
        error $
          unlines
            [ "intExp: non-function type in application",
              prettyString expr
            ]
intExp expr@(TApp e ts _ _) =
  tApply =<< intExp e
  where
    tApply :: Val -> InterpM Val
    tApply (ValVar f) = do
      f' <- lookupVal f
      case f' of
        ValTFun func -> func ts
        _ ->
          error $
            unlines
              [ "intExp: non-type function value in type application",
                prettyString expr
              ]
    tApply (ValTFun f) = f ts
    tApply (ValArray shape fs) = do
      ValArray shape <$> mapM tApply fs
    tApply _ =
      error $
        unlines
          [ "intExp: non-type function value in type application",
            prettyString expr
          ]
intExp expr@(IApp e is _ _) = do
  e' <- intExp e
  is' <- mapM intExtent is
  iapply e' is'
  where
    iapply :: Val -> [Either Int [Int]] -> InterpM Val
    iapply (ValIFun f) extents = f extents
    iapply (ValArray shape fs) shapes = do
      ValArray shape <$> mapM (`iapply` shapes) fs
    iapply _ _ =
      error $
        unlines
          [ "intExp: non-index function value in index application",
            prettyString expr
          ]
intExp expr@(Unbox is x_e box e _ _) = do
  box' <- intExp box
  arrayValView box' $ \(ns, boxes) -> do
    elems <- mapM unbox boxes
    pure $ ValArray (length elems : ns) elems
  where
    unbox (ValBox extents v _) = do
      extents' <- mapM intExtent extents
      binds ibind (zip (map unExtentParam is) extents') $
        bind x_e v $
          intExp e
    unbox v =
      error $
        unlines
          [ "unbox: non-box value",
            prettyString v,
            "in",
            prettyString expr
          ]
intExp expr@(Let bs e _ _) =
  foldr intBind (intExp e) bs
  where
    intBind :: Bind -> InterpM a -> InterpM a
    intBind (BindVal v _ e _) m = do
      val <- intExp e
      bind v val m
    intBind (BindFun f params _ body _) m = do
      flip (bind f) m $ ValFun $ \vals ->
        binds bind (zip (map fst params) vals) $
          intExp body
    intBind (BindTFun f params _ body _) m = do
      flip (bind f) m $ ValTFun $ \ts ->
        binds tbind (zip (map unTypeParam params) ts) $
          intExp body
    intBind (BindIFun f params _ body _) m = do
      flip (bind f) m $ ValIFun $ \is ->
        binds ibind (zip (map unExtentParam params) is) $
          intExp body
    intBind _ m = m

-- | Interpret a 'Dim'.
intDim :: Dim -> InterpM Int
intDim (DimVar d) = lookupDim d
intDim (DimN d) = pure d
intDim (Add ds) = sum <$> mapM intDim ds

-- | Interpret a 'Shape'.
intShape :: Shape -> InterpM [Int]
intShape (ShapeVar s) = lookupShape s
intShape (ShapeDim d) = pure <$> intDim d
intShape (Concat ss) = concat <$> mapM intShape ss

-- | Interpret an 'Extent'.
intExtent :: Extent -> InterpM (Either Int [Int])
intExtent = mapExtent (fmap Left . intDim) (fmap Right . intShape)

-- | Interpret an 'Atom'.
intAtom :: Atom -> InterpM Val
intAtom (Base b _ _) = pure $ ValBase b
intAtom (Lambda ps e _ _) = do
  env <- ask
  pure $ ValFun $ \vs ->
    local (const env) $
      binds bind (zip (map fst ps) vs) $
        intExp e
intAtom (TLambda ps e _ _) = do
  env <- ask
  pure $ ValTFun $ \ts ->
    local (const env) $
      binds tbind (zip (map unTypeParam ps) ts) $
        intExp e
intAtom (ILambda ps e _ _) = do
  env <- ask
  pure $ ValIFun $ \extents ->
    local (const env) $
      binds ibind (zip (map unExtentParam ps) extents) $
        intExp e
intAtom (Box is e t _) =
  ValBox is <$> intExp e <*> pure t

-- | Replicates cells to make shapes match for function application.
lift :: [Int] -> Val -> ([[Int]], [Val]) -> (Val, [Val])
lift pframe (ValArray fsshape fs) (sparams, argvs) =
  (ValArray pframe fs', lifted_argvs)
  where
    -- replicate the function array to match the frame
    n_fun_reps = product pframe `div` product fsshape
    fs' = concat $ rep n_fun_reps $ split 1 fs

    -- lift each argument to the new frame
    lifted_argvs = zipWith liftArg sparams argvs

    liftArg :: [Int] -> Val -> Val
    liftArg shape_param arg =
      arrayValView arg $ \(shapeArg, as) ->
        -- Number of cells the argument expects.
        let n_arg_cell = product shape_param
            -- Number of times the argument needs to be replicated.
            n_arg_reps = product pframe `div` product (shapeArg \\ shape_param)
         in ValArray
              (pframe <> shape_param)
              (concat $ rep n_arg_reps $ split n_arg_cell as)
lift _ v _ = error $ prettyString v

-- | Performs a function application between an array
-- of functions and arguments that have compatible shapes.
apMap :: Val -> ([[Int]], [Val]) -> InterpM Val
apMap v (sparams, argvs) =
  arrayValView v $ \(shape_f, fs) ->
    ValArray shape_f <$> zipWithM apply_fun fs arg_splits
  where
    apply_fun f args =
      apply f (zipWith ValArray sparams args)

    arg_splits =
      L.transpose $ zipWith arg_split sparams argvs

    -- Splits the argument according to the shape of the parameter.
    arg_split shape_param arg =
      arrayValView arg $
        split (product shape_param) . snd

    apply :: Val -> [Val] -> InterpM Val
    apply (ValVar f) args = do
      f' <- lookupVal f
      case f' of
        ValFun func -> func args
        _ -> throwError $ "cannot apply: " <> prettyText f'
    apply (ValArray shape fs) args = do
      vs <- zipWithM apply fs (map pure args)
      pure $ ValArray shape vs
    apply (ValFun f) args = f args
    apply val _ =
      error $
        unlines
          [ "apply: non-function value",
            prettyString val
          ]
