module Interpreter where

import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.Reader hiding (lift)
import Control.Monad.Trans.Except
import Data.Foldable (find)
import Data.List qualified
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Interpreter.Intrinsics
import Interpreter.Value hiding (Val)
import Interpreter.Value qualified as Value
import Parser qualified as P
import Pass
import Prop hiding ((\\))
import Syntax hiding (ArrayType, Dim, ISpace, Shape, Type, TypeExp)
import Syntax qualified
import System.IO.Unsafe (unsafePerformIO)
import TypeCheck qualified as TC
import Uniquify qualified as U
import Util
import VName

type Dim = Syntax.Dim VName

type Val = Value.Val InterpM

type Shape = Syntax.Shape VName

type Type = Syntax.Type VName

type ISpace = Syntax.ISpace VName

type TypeExp = Syntax.TypeExp VName

type ArrayType = Syntax.ArrayType VName

interpret :: T.Text -> [Val] -> Prog -> PassM Val
interpret entry args (Prog decls) =
  liftEither $
    runReader (runExceptT $ runInterpM $ intProg entry args decls) initEnv

interpretExp :: Exp -> PassM Val
interpretExp e =
  liftEither $
    runReader (runExceptT $ runInterpM $ intExp e) initEnv

intProg :: T.Text -> [Val] -> [Decl] -> InterpM Val
intProg entry args decls = foldr intDecl runEntry decls
  where
    runEntry =
      case [(params, body) | Entry v params _ body _ _ <- decls, varName v == entry] of
        [] -> throwError $ "interpret: program has no " <> entry <> " entry"
        [(params, body)]
          | length params == length args -> do
              env <- ask
              f <- curryBind ValFun (bindEnv . patVar) body env params
              foldM apply f args
          | otherwise ->
              throwError $
                T.unlines
                  [ "interpret: " <> entry <> " expects",
                    T.show $ length params,
                    "arguments but got",
                    T.show $ length args
                  ]
        _ -> throwError $ "interpret: multiple " <> entry <> " entries"
    apply (ValFun g) arg = g arg
    apply _ _ = throwError $ "interpret: " <> entry <> " over-applied"

intDecl :: Decl -> InterpM a -> InterpM a
intDecl (Def b) m = intBind b m
intDecl (Entry v params _ body _ _) m = do
  env <- ask
  flip (bind v) m =<< curryBind ValFun (bindEnv . patVar) body env params

intBind :: Bind -> InterpM a -> InterpM a
intBind (BindVal v _ e' _) m = do
  val <- intExp e'
  bind v val m
intBind (BindFun f params _ body _ _) m = do
  env <- ask
  fun <- curryBind ValFun (bindEnv . patVar) body env $ NE.toList params
  bind f fun m
intBind (BindTFun f params _ body _ _) m = do
  env <- ask
  fun <- curryBind ValTFun (tbindEnv . unTypeParam) body env $ NE.toList params
  bind f fun m
intBind (BindIFun f params _ body _ _) m = do
  env <- ask
  fun <- curryBind ValIFun (ibindEnv . unISpaceParam) body env $ NE.toList params
  bind f fun m
intBind _ m = m

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

instance Semigroup Env where
  Env vs1 ts1 ds1 ss1 <> Env vs2 ts2 ds2 ss2 =
    Env (vs1 <> vs2) (ts1 <> ts2) (ds1 <> ds2) (ss1 <> ss2)

instance Monoid Env where
  mempty = Env mempty mempty mempty mempty

-- | The initial environment.
initEnv :: Env
initEnv = Env m tm mempty mempty
  where
    intrinsicList = intrinsics :: Intrinsics InterpM
    m0 =
      M.fromList $
        map (\(IntrinsicVal v _ val) -> (v, val)) intrinsicList
    tm =
      M.fromList $
        map (\(IntrinsicVal v t _) -> (v, Syntax.ArrayType t)) intrinsicList
    rfVname =
      case [v | IntrinsicVal v _ _ <- intrinsicList, varName v == "read-file"] of
        v : _ -> v
        [] -> error "initialEnv: missing read-file intrinsic"
    rfImpl =
      ValTFun $ \_ ->
        pure $ ValIFun $ \_ ->
          pure $ ValIFun $ \_ ->
            pure $ ValFun $ \filename ->
              let file = valToString filename
               in pure $ unsafePerformIO $ do
                    input <- TIO.readFile file
                    let mv = do
                          parsed <- P.parseExp "<read-file>" input
                          Pass.runPass $
                            U.uniquifyExp parsed
                              >>= TC.checkExp
                              >>= interpretExp
                    case mv of
                      Left err -> error $ "read-file: " <> T.unpack err
                      Right v -> pure v
    m = M.insert rfVname rfImpl m0

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

bindEnv :: VName -> Val -> Env -> Env
bindEnv v val env = env {envMap = M.insert v val $ envMap env}

-- | Locally bind a variable to a value.
bind :: VName -> Val -> InterpM a -> InterpM a
bind v val = local (bindEnv v val)

-- | Extend an environment with a type variable binding.
tbindEnv :: VName -> Type -> Env -> Env
tbindEnv v t env = env {envTMap = M.insert v t $ envTMap env}

-- | Locally bind a type variable to a type.
tbind :: VName -> Type -> InterpM a -> InterpM a
tbind v t = local (tbindEnv v t)

-- | Extend an environment with an index variable binding.
ibindEnv :: VName -> Either Int [Int] -> Env -> Env
ibindEnv v (Left d) env = env {envDMap = M.insert v d $ envDMap env}
ibindEnv v (Right s) env = env {envSMap = M.insert v s $ envSMap env}

-- | Locally bind an index variable to a shape literal.
ibind :: VName -> Either Int [Int] -> InterpM a -> InterpM a
ibind v i = local (ibindEnv v i)

-- | Many binds.
binds :: (v -> val -> a -> a) -> [(v, val)] -> a -> a
binds _ [] m = m
binds f ((v, val) : vvals) m =
  f v val $ binds f vvals m

-- | Build a curried function value.
curryBind ::
  ((x -> InterpM Val) -> Val) ->
  (p -> x -> Env -> Env) ->
  Exp ->
  Env ->
  [p] ->
  InterpM Val
curryBind wrap ext body = doBind
  where
    doBind e [] = local (const e) $ intExp body
    doBind e (p : ps) = pure $ wrap $ \x -> doBind (ext p x e) ps

-- | Intepret an expression.
intExp :: Exp -> InterpM Val
intExp (Var v _ _) = lookupVal v
intExp (Array shape as _ _) =
  ValArray shape . NE.toList <$> mapM intAtom as
intExp (EmptyArray shape _ _ _) =
  pure $ ValArray shape mempty
intExp (Frame shape es _ _) =
  ValArray shape . NE.toList <$> mapM intExp es
intExp (EmptyFrame shape _ _ _) =
  pure $ ValArray shape mempty
intExp expr@(App f e (Info (_, pframe)) _) = do
  pframe' <- intShape pframe
  f' <- arrayifyVal <$> intExp f
  argv <- intExp e
  arrayTypeView (typeOf f) $ \(t, _) ->
    case t of
      pts :-> _ -> do
        sparam <- intShape $ shapeOf pts
        let (f'', argv') = lift pframe' f' (sparam, argv)
        apMap f'' (sparam, argv')
      _ ->
        error $
          unlines
            [ "intExp: non-function type in application",
              prettyString expr
            ]
intExp expr@(TApp e t _ _) =
  tApply =<< intExp e
  where
    tApply :: Val -> InterpM Val
    tApply (ValVar f) = do
      f' <- lookupVal f
      case f' of
        ValTFun func -> func t
        _ ->
          error $
            unlines
              [ "intExp: non-type function value in type application",
                prettyString expr
              ]
    tApply (ValTFun f) = f t
    tApply (ValArray shape fs) = do
      ValArray shape <$> mapM tApply fs
    tApply _ =
      error $
        unlines
          [ "intExp: non-type function value in type application",
            prettyString expr
          ]
intExp expr@(IApp e i _ _) = do
  e' <- intExp e
  i' <- intISpace i
  iapply e' i'
  where
    iapply :: Val -> Either Int [Int] -> InterpM Val
    iapply (ValIFun f) ispace = f ispace
    iapply (ValArray shape fs) ispace = do
      ValArray shape <$> mapM (`iapply` ispace) fs
    iapply _ _ =
      error $
        unlines
          [ "intExp: non-index function value in index application",
            prettyString expr
          ]
intExp expr@(Unbox ep x_e box e _ _) = do
  box' <- intExp box
  let (ns, boxes) = asArray box'
  elems <- mapM unbox boxes
  pure $ collapse $ ValArray (valShapeOf box' <> ns) elems
  where
    unbox (ValBox [ispace] v) =
      ibind (unISpaceParam ep) ispace $
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
intExp (Let bs e _ _) =
  foldr intBind (intExp e) bs
intExp expr@(Struct s (Info (_, pframe)) _) = do
  let (fnames, shps, es) = neUnzip3 s
  pframe' <- intShape pframe
  let pcnt = product pframe'
  shps' <- mapM intShape $ NE.toList shps
  es' <- mapM intExp $ NE.toList es
  replFieldsEs <-
    zipWithM
      ( \shp' v' -> case v' of
          ValArray inShp vs -> do
            let splitVs = split (product inShp `div` product shp') (product shp') vs
            let replFactor = pcnt `div` (product inShp `div` product shp')
            pure $ concat $ replicate replFactor $ map (ValArray shp') splitVs
          _ ->
            error $
              unlines
                [ "struct construction: rhs of a field did not evaluate to an array",
                  prettyString v',
                  "in",
                  prettyString expr
                ]
      )
      shps'
      es'
  -- fields - frame -> frame - fields
  let tReplFields = Data.List.transpose replFieldsEs
  let recs = map (ValRecord . zip (NE.toList fnames)) tReplFields
  pure $ ValArray pframe' recs
intExp expr@(FieldProj e f _ _) = do
  e' <- intExp e
  let (ns, recs) = asArray e'
  case recs of
    [] ->
      error $
        unlines
          [ "field proj: empty array of records in projection",
            prettyString e,
            "in",
            prettyString expr
          ]
    rs@(r : _) | Just vs' <- mapM projMaybe rs -> do
      pure $ collapse $ ValArray (ns <> valShapeOf r) vs'
      where
        projMaybe (ValRecord fs) = case find (\(fn, _) -> fn == f) fs of
          Just (_, v) -> Just v
          Nothing -> Nothing
        projMaybe _ = Nothing
    _ ->
      error $
        unlines
          [ "field proj: records do not have the right field",
            prettyString e,
            "in",
            prettyString expr
          ]

-- | Interpret a 'Dim'. Variables are looked up in the environment; the result
-- must be non-negative (subtraction may produce negatives in subterms).
intDim :: Dim -> InterpM Int
intDim d = do
  x <- dimToInt lookupDim d
  when (x < 0) $
    throwError $
      "intDim: negative dimension: " <> prettyText x <> " from " <> prettyText d
  pure x

-- | Interpret a 'Shape'.
intShape :: Shape -> InterpM [Int]
intShape = shapeToInts intDim lookupShape

-- | Interpret an 'ISpace'.
intISpace :: ISpace -> InterpM (Either Int [Int])
intISpace = mapISpace (fmap Left . intDim) (fmap Right . intShape)

-- | Interpret a type expression.
intTypeExp :: TypeExp -> ArrayType
intTypeExp = fromJust . convertArrayTypeExp

-- | Interpret an 'Atom'.
intAtom :: Atom -> InterpM Val
intAtom (Base b _ _) = pure $ ValBase b
intAtom (Lambda p e _ _) = do
  env <- ask
  pure $ ValFun $ \v ->
    local (const env) $
      bind (patVar p) v $
        intExp e
intAtom (TLambda p e _ _) = do
  env <- ask
  pure $ ValTFun $ \t ->
    local (const env) $
      tbind (unTypeParam p) t $
        intExp e
intAtom (ILambda p e _ _) = do
  env <- ask
  pure $ ValIFun $ \ispace ->
    local (const env) $
      ibind (unISpaceParam p) ispace $
        intExp e
intAtom (Box ispace e _ _ _) = do
  ispace' <- intISpace ispace
  ValBox [ispace'] <$> intExp e

-- | Replicates cells to make shapes match for function application.
lift :: [Int] -> Val -> ([Int], Val) -> (Val, Val)
lift pframe (ValArray fsshape fs) (sparam, argv) =
  (ValArray pframe fs', liftArg sparam argv)
  where
    -- replicate the function array to match the frame
    n_fun_reps = fromIntegral $ product pframe `div` product fsshape
    fs' = concat $ rep n_fun_reps $ split (length fs) 1 fs

    liftArg :: [Int] -> Val -> Val
    liftArg shape_param arg =
      let (shapeArg, as) = asArray arg
          arg_frame = shapeArg \\ shape_param
          -- Number of cells the argument expects.
          n_arg_cell = product shape_param
          -- Number of times the argument needs to be replicated.
          n_arg_reps = product pframe `div` product arg_frame
       in ValArray
            (pframe <> shape_param)
            (concat $ rep n_arg_reps $ split (product arg_frame) n_arg_cell as)
lift _ v _ = error $ prettyString v

-- | Performs a function application between an array
-- of functions and arguments that have compatible shapes.
apMap :: Val -> ([Int], Val) -> InterpM Val
apMap v (sparam, argv) =
  let (shape_f, fs) = asArray v
      cells = split (length fs) (product sparam) (snd $ asArray argv)
   in collapse . ValArray shape_f <$> zipWithM apply_fun fs cells
  where
    apply_fun f cell = apply f (ValArray sparam cell)

    apply :: Val -> Val -> InterpM Val
    apply (ValVar f) arg = do
      f' <- lookupVal f
      case f' of
        ValFun func -> func arg
        _ -> throwError $ "cannot apply: " <> prettyText f'
    apply (ValArray shape fs) arg =
      ValArray shape <$> zipWithM apply fs [arg]
    apply (ValFun f) arg = f arg
    apply val _ =
      error $
        unlines
          [ "apply: non-function value",
            prettyString val
          ]
