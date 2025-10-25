module Futhark (FutharkM, compile) where

import Control.Monad.Error.Class
import Control.Monad.Reader hiding (lift)
import Control.Monad.State hiding (State)
import Control.Monad.Trans.Except
import Data.Text qualified as T
import Prettyprinter
import Prettyprinter.Render.Text
import RemoraPrelude (Prelude)
import Syntax hiding (Atom, Dim, Exp, Idx, Shape, Type, (\\))
import Syntax qualified
import VName

type Dim = Syntax.Dim VName

type Exp = Syntax.Exp Info VName

type Atom = Syntax.Atom Info VName

type Shape = Syntax.Shape VName

type Type = Syntax.Type VName

type FutharkExp = Doc ()

type FutharkParam = Doc ()

data FutharkType
  = FutharkBool
  | Int32
  | Int64
  | Float64
  | FutharkArray [FutharkDim] FutharkType
  deriving (Show)

prettyType :: FutharkType -> Doc ()
prettyType FutharkBool = "bool"
prettyType Int32 = "i32"
prettyType Int64 = "i64"
prettyType Float64 = "f64"
prettyType (FutharkArray shape t) =
  hsep (map brackets shape) <> prettyType t

prettyTypeNoDims :: FutharkType -> Doc ()
prettyTypeNoDims FutharkBool = "bool"
prettyTypeNoDims Int32 = "i32"
prettyTypeNoDims Int64 = "i64"
prettyTypeNoDims Float64 = "f64"
prettyTypeNoDims (FutharkArray shape t) =
  hsep (map (const "[]") shape) <> prettyTypeNoDims t

type FutharkDim = Doc ()

type FutharkStm = Doc ()

type FutharkStms = Doc ()

type FutharkVar = Doc ()

type FutharkFun = Doc ()

type FutharkBody = Doc ()

data Env = Env

type Error = T.Text

data State = State
  { stateStms :: FutharkStms,
    stateCounter :: Int,
    stateFuns :: [FutharkFun]
  }

-- | The Futhark generator monad.
newtype FutharkM a = FutharkM {runFutharkM :: StateT State (ReaderT Env (Except Error)) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader Env,
      MonadError Error,
      MonadState State
    )

emit :: FutharkStm -> FutharkM ()
emit stm = modify $ \s -> s {stateStms = vsep [stateStms s, stm]}

collect :: FutharkM a -> FutharkM (a, FutharkStms)
collect m = do
  old_stms <- gets stateStms
  x <- m
  stms <- gets stateStms
  modify $ \s -> s {stateStms = old_stms}
  pure (x, stms)

newVar :: FutharkM FutharkVar
newVar = do
  x <- gets stateCounter
  modify $ \s -> s {stateCounter = succ x}
  pure $ "v_" <> pretty x

bind :: FutharkType -> FutharkExp -> FutharkM FutharkVar
bind t e = do
  v <- newVar
  emit $ "let" <+> braces (v <> ":" <+> prettyType t) <+> "=" <+> e
  pure v

mkBody :: FutharkM [FutharkVar] -> FutharkM FutharkBody
mkBody m = do
  (vs, stms) <- collect m
  pure $ vsep [stms, "in" <+> braces (mconcat $ punctuate ", " vs)]

addFunction :: FutharkFun -> FutharkM ()
addFunction fun = modify $ \s -> s {stateFuns = stateFuns s ++ [fun]}

findRet :: Type -> Type
findRet (_ :-> t) = findRet t
findRet t = t

-- Assumes the lambda has no free variables.
liftLambda :: [(VName, Type)] -> Exp -> Type -> FutharkM FutharkVar
liftLambda params body ret = do
  params' <- mapM compileParam params
  body' <- mkBody $ pure <$> compileExp body
  ret' <- compileType $ findRet ret
  fname <- newVar
  addFunction $
    vsep
      [ "fun" <+> fname <+> parens (mconcat $ punctuate ", " params'),
        "  :" <+> braces (prettyType ret') <+> "= {",
        indent 2 body',
        "}"
      ]
  pure fname

compileDim :: Dim -> FutharkM FutharkDim
compileDim (DimN x) = pure $ pretty x <> "i64"
compileDim d = error $ "compileDim: unhandled:\n" ++ show d

compileShape :: Shape -> FutharkM [FutharkDim]
compileShape (ShapeDim d) = pure <$> compileDim d
compileShape (Concat ds) = concat <$> mapM compileShape ds
compileShape s = error $ "compileShape: unhandled:\n" ++ show s

compileType :: Type -> FutharkM FutharkType
compileType Bool = pure FutharkBool
compileType Int = pure Int32
compileType Float = pure Float64
compileType (TArr t shape) = do
  t' <- compileType t
  shape' <- compileShape shape
  pure $ FutharkArray shape' t'
compileType t = error $ "compileType: unhandled:\n" ++ show t

compileParam :: (VName, Type) -> FutharkM FutharkParam
compileParam (v, t) = do
  t' <- compileType t
  pure $ pretty v <> ":" <+> prettyType t'

compileAtom :: Atom -> FutharkM FutharkExp
compileAtom (Base (BoolVal True) _ _) = pure "true"
compileAtom (Base (BoolVal False) _ _) = pure "false"
compileAtom (Base (IntVal x) _ _) = pure $ pretty x <> "i32"
compileAtom (Base (FloatVal x) _ _) = pure $ pretty x <> "f64"
compileAtom (Lambda params body (Info t) _) = do
  liftLambda params body t
compileAtom e = error $ "compileAtom: unhandled:\n" ++ show e

map1 :: Int -> Type -> FutharkVar -> [FutharkVar] -> FutharkType -> FutharkM FutharkVar
map1 dim (pts :-> r) f xss@[_, _] t = do
  x' <- newVar
  y' <- newVar
  pts' <- mapM compileType pts
  r' <- compileType r
  body <-
    mkBody $ pure <$> bind r' ("add32" <> parens (x' <> "," <> y'))
  bind t $
    vsep
      [ "map"
          <> parens
            ( vsep
                [ pretty dim <> "i64,",
                  braces (hsep (punctuate "," xss)) <> ",",
                  "\\"
                    <+> braces
                      ( vsep
                          ( punctuate
                              ","
                              (zipWith (\x xt -> x <+> ":" <+> prettyType xt) [x', y'] pts')
                          )
                      ),
                  ":" <+> braces (prettyType r') <+> "->",
                  body
                ]
            )
      ]

compileExp :: Exp -> FutharkM FutharkVar
compileExp (Array [] [x] _ _) = compileAtom x
compileExp e@(Array [_] elems (Info (TArr elem_t _)) _) = do
  elems' <- mapM compileAtom elems
  t <- compileType $ typeOf e
  elem_t' <- compileType elem_t
  bind t $
    "["
      <> mconcat (punctuate "," elems')
      <> "]"
      <+> ":"
      <+> "[]"
      <> prettyType elem_t'
compileExp (Var v _ _) =
  pure $ pretty v
compileExp (App (Var v _ _) [x, y] (Info (t, pframe)) _)
  | Just v' <- lookup (varName v, t) binops = do
      x' <- compileExp x
      y' <- compileExp y
      t' <- compileType t
      bind t' $ v' <> parens (x' <> "," <> y')
  where
    binops =
      [ (("+", TArr Int (Concat [])), "add32"),
        (("-", TArr Int (Concat [])), "sub32")
      ]
compileExp e@(App f xs (Info (t, pframe)) _) = do
  f' <- compileExp f
  xs' <- traverse compileExp xs
  t' <- compileType t
  case intShape $ normShape pframe of
    [] ->
      bind t' $
        vsep
          [ "apply" <+> f' <+> parens (mconcat $ punctuate "," xs'),
            ":" <+> braces (prettyType t')
          ]
    [d] -> map1 d (typeOf f) f' xs' t'
    _ -> error $ "compileExp: unhandled:\n" ++ show e
  where
    intDim :: Dim -> Int
    intDim (DimVar d) = error "intDim: AAAAAAAAAAAAAAAAAAA"
    intDim (DimN d) = d
    intDim (Add ds) = sum $ map intDim ds

    intShape :: Shape -> [Int]
    intShape (ShapeVar s) = error "intShape: AAAAAAAAAAAAAAAAAAA"
    intShape (ShapeDim d) = pure $ intDim d
    intShape (Concat ss) = concat $ map intShape ss
compileExp e = error $ "compileExp: unhandled:\n" ++ show e

wrapInMain :: ((FutharkVar, FutharkType), State) -> T.Text
wrapInMain ((e, ret), State stms counter funs) =
  renderStrict . layoutPretty defaultLayoutOptions $
    vcat $
      "name_source" <+> braces (pretty counter)
        : funs
        ++ [ "entry (\"main\", {}, {"
               <> prettyTypeNoDims ret
               <> "}) entry_main () : {"
               <> prettyType ret
               <> "} = {",
             indent 2 (vcat [stms, "in" <+> braces e]),
             "}"
           ]

-- | Turn Remora into Futhark.
compile :: Prelude VName FutharkM -> Exp -> Either Error T.Text
compile _prelude e =
  wrapInMain
    <$> runExcept
      ( runReaderT
          ( runStateT
              ( runFutharkM
                  ( (,)
                      <$> compileExp e
                      <*> compileType (typeOf e)
                  )
              )
              initialState
          )
          Env
      )
  where
    initialState = State mempty 0 mempty
