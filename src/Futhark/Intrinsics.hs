module Futhark.Intrinsics (compileIntrinsic) where

import Control.Monad (zipWithM, (<=<))
import Data.List (singleton)
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Futhark.Construct qualified as F
import Futhark.IR.SOACS qualified as F
import Futhark.Monad
import Intrinsics qualified
import Primitive
import Syntax hiding (Add, ArrayType, Mul, Sub)
import Util
import VName

type Translate = [Arg] -> ArrayType -> FutharkM [F.SubExp]

type TranslateValue = [Arg] -> ArrayType -> FutharkM F.SubExp

compileIntrinsic :: VName -> Maybe Translate
compileIntrinsic v = translate (varName v) <$ Intrinsics.intrinsics M.!? v

translate :: Text -> Translate
translate name as t =
  case scalarOps M.!? name of
    Just op -> singleton <$> op as t
    Nothing -> intrinsic name as t

scalarOps :: Map Text TranslateValue
scalarOps =
  M.fromList $
    [(prettyText op, binOpTranslate op) | (op, _) <- binOps]
      ++ [(prettyText op, unOpTranslate op) | (op, _) <- unOps]

binOpTranslate :: BinOp -> TranslateValue
binOpTranslate op [Arg x, Arg y] _ = compileBinOp op (argSExp x) (argSExp y)
binOpTranslate op as _ = badApp (prettyText op) as

unOpTranslate :: UnOp -> TranslateValue
unOpTranslate op [Arg x] _ = compileUnOp op (argSExp x)
unOpTranslate op as _ = badApp (prettyText op) as

badApp :: Text -> [Arg] -> FutharkM a
badApp name as =
  error $
    "Futhark.Intrinsics: bad application of "
      ++ T.unpack name
      ++ " to "
      ++ show (length as)
      ++ " arguments"

compileBinOp :: BinOp -> F.SubExp -> F.SubExp -> FutharkM F.SubExp
compileBinOp Add = binOp $ F.Add F.Int64 F.OverflowWrap
compileBinOp Sub = binOp $ F.Sub F.Int64 F.OverflowWrap
compileBinOp Mul = binOp $ F.Mul F.Int64 F.OverflowWrap
compileBinOp Div = binOp $ F.SDiv F.Int64 F.Safe
compileBinOp Pow = binOp $ F.Pow F.Int64
compileBinOp Mod = binOp $ F.SMod F.Int64 F.Safe
compileBinOp Max = binOp $ F.SMax F.Int64
compileBinOp Min = binOp $ F.SMin F.Int64
compileBinOp BitAnd = binOp $ F.And F.Int64
compileBinOp BitOr = binOp $ F.Or F.Int64
compileBinOp BitXor = binOp $ F.Xor F.Int64
compileBinOp Shl = binOp $ F.Shl F.Int64
compileBinOp Shr = binOp $ F.AShr F.Int64
compileBinOp FAdd = binOp $ F.FAdd F.Float32
compileBinOp FSub = binOp $ F.FSub F.Float32
compileBinOp FMul = binOp $ F.FMul F.Float32
compileBinOp FDiv = binOp $ F.FDiv F.Float32
compileBinOp FPow = binOp $ F.FPow F.Float32
compileBinOp FMax = binOp $ F.FMax F.Float32
compileBinOp FMin = binOp $ F.FMin F.Float32
compileBinOp Eq = cmpOp $ F.CmpEq $ F.IntType F.Int64
compileBinOp Neq = notCmpOp $ F.CmpEq $ F.IntType F.Int64
compileBinOp Lt = cmpOp $ F.CmpSlt F.Int64
compileBinOp Gt = flip $ cmpOp $ F.CmpSlt F.Int64
compileBinOp Le = cmpOp $ F.CmpSle F.Int64
compileBinOp Ge = flip $ cmpOp $ F.CmpSle F.Int64
compileBinOp FEq = cmpOp $ F.CmpEq $ F.FloatType F.Float32
compileBinOp FNeq = notCmpOp $ F.CmpEq $ F.FloatType F.Float32
compileBinOp FLt = cmpOp $ F.FCmpLt F.Float32
compileBinOp FGt = flip $ cmpOp $ F.FCmpLt F.Float32
compileBinOp FLe = cmpOp $ F.FCmpLe F.Float32
compileBinOp FGe = flip $ cmpOp $ F.FCmpLe F.Float32
compileBinOp And = binOp F.LogAnd
compileBinOp Or = binOp F.LogOr
compileBinOp BEq = cmpOp $ F.CmpEq F.Bool
compileBinOp BNeq = notCmpOp $ F.CmpEq F.Bool

compileUnOp :: UnOp -> F.SubExp -> FutharkM F.SubExp
compileUnOp Sqrt x = callBuiltin "sqrt32" (F.FloatType F.Float32) [x]
compileUnOp FSqrt x = callBuiltin "sqrt32" (F.FloatType F.Float32) [x]
compileUnOp FLn x = callBuiltin "log32" (F.FloatType F.Float32) [x]
compileUnOp FTanh x = callBuiltin "tanh32" (F.FloatType F.Float32) [x]
compileUnOp BitNot x = unOp (F.Complement F.Int64) x
compileUnOp Popc x =
  convOp (F.SExt F.Int32 F.Int64) =<< callBuiltin "popc64" (F.IntType F.Int32) [x]
compileUnOp Not x = unOp (F.Neg F.Bool) x
compileUnOp IntToFloat x = convOp (F.SIToFP F.Int64 F.Float32) x
compileUnOp IntToBool x = convOp (F.IToB F.Int64) x
compileUnOp BoolToInt x = convOp (F.BToI F.Int64) x
compileUnOp BoolToFloat x =
  convOp (F.SIToFP F.Int64 F.Float32) =<< convOp (F.BToI F.Int64) x
compileUnOp Truncate x = convOp (F.FPToSI F.Float32 F.Int64) x
compileUnOp Round x =
  convOp (F.FPToSI F.Float32 F.Int64) =<< callBuiltin "round32" (F.FloatType F.Float32) [x]
compileUnOp Ceiling x =
  convOp (F.FPToSI F.Float32 F.Int64) =<< callBuiltin "ceil32" (F.FloatType F.Float32) [x]
compileUnOp Floor x =
  convOp (F.FPToSI F.Float32 F.Int64) =<< callBuiltin "floor32" (F.FloatType F.Float32) [x]

binOp :: F.BinOp -> F.SubExp -> F.SubExp -> FutharkM F.SubExp
binOp o x y = F.letSubExp "binop" $ F.BasicOp $ F.BinOp o x y

cmpOp :: F.CmpOp -> F.SubExp -> F.SubExp -> FutharkM F.SubExp
cmpOp c x y = F.letSubExp "cmp" $ F.BasicOp $ F.CmpOp c x y

notCmpOp :: F.CmpOp -> F.SubExp -> F.SubExp -> FutharkM F.SubExp
notCmpOp c x y =
  F.letSubExp "not" . F.BasicOp . F.UnOp (F.Neg F.Bool) =<< cmpOp c x y

unOp :: F.UnOp -> F.SubExp -> FutharkM F.SubExp
unOp o x = F.letSubExp "unop" $ F.BasicOp $ F.UnOp o x

convOp :: F.ConvOp -> F.SubExp -> FutharkM F.SubExp
convOp c x = F.letSubExp "conv" $ F.BasicOp $ F.ConvOp c x

callBuiltin :: Text -> F.PrimType -> [F.SubExp] -> FutharkM F.SubExp
callBuiltin name ret args =
  F.letSubExp (F.nameFromText name) $
    F.Apply (F.nameFromText name) [(x, F.Observe) | x <- args] [(F.Prim ret, mempty)] F.Safe

argVar :: F.Name -> SExpArg -> FutharkM F.VName
argVar name = F.letExp name . F.BasicOp . F.SubExp . argSExp

argVars :: F.Name -> SExpArg -> FutharkM [F.VName]
argVars name = mapM (F.letExp name . F.BasicOp . F.SubExp) . argSExps

mapValues ::
  (F.VName -> F.Type -> FutharkM F.SubExp) ->
  SExpArg ->
  ArrayType ->
  FutharkM [F.SubExp]
mapValues f xs t = do
  ts <- compileArrayTypes t
  fxs <- argVars "xs" xs
  zipWithM f fxs ts

reshapeTo :: F.Name -> ArrayType -> F.VName -> FutharkM F.SubExp
reshapeTo name t v = do
  rank <- F.arrayRank <$> F.lookupType v
  shape <- F.arrayShape <$> compileArrayType t
  F.letSubExp name $
    F.BasicOp $
      F.Reshape v $
        F.NewShape [F.DimSplice 0 rank shape] shape

compileReduce :: F.Reduce F.SOACS -> SExpArg -> FutharkM F.SubExp
compileReduce red xs = do
  fxs <- argVar "xs" xs
  n <- F.arraySize 0 <$> compileArrayType (argType xs)
  form <- F.reduceSOAC [red]
  F.letSubExp "reduce" $ F.Op $ F.Screma n [fxs] form

compileReverse :: F.VName -> FutharkM F.VName
compileReverse xs = do
  txs <- F.lookupType xs
  let n = F.arraySize 0 txs
  start <-
    F.letSubExp "start"
      =<< F.eBinOp (F.Sub F.Int64 F.OverflowWrap) (F.eSubExp n) (F.eSubExp $ constInt64 1)
  F.letExp "reverse" $
    F.BasicOp $
      F.Index xs $
        F.fullSlice txs [F.DimSlice start n (constInt64 (-1))]

compileFold :: F.Name -> F.Lambda F.SOACS -> F.VName -> F.VName -> FutharkM F.SubExp
compileFold name op acc xs = do
  n <- F.arraySize 0 <$> F.lookupType xs
  i <- F.newVName "i"
  accParam <- F.newParam "acc" . (`F.toDecl` F.Nonunique) =<< F.lookupType acc
  let form = F.ForLoop i F.Int64 n
  body <-
    F.localScope (F.scopeOfLoopForm form <> F.scopeOfFParams [accParam]) $
      mkBody $
        map F.resSubExp
          <$> F.eLambda
            op
            [ F.eSubExp $ F.Var $ F.paramName accParam,
              F.eIndex xs [F.eSubExp $ F.Var i]
            ]
  F.letSubExp name $ F.Loop [(accParam, F.Var acc)] form body

compileFlatten :: SExpArg -> FutharkM SExpArg
compileFlatten xs =
  case intShape $ arrayTypeShape $ argType xs of
    [_] -> pure xs
    ds -> do
      flat <- flatten ds
      pure $
        SExpArg [] [F.Var flat] $
          arrayTypeAtom (argType xs) :@ ShapeDim (DimN $ product ds)
  where
    flatShape ds = F.Shape [constInt64 $ product ds]

    flatten [] =
      F.letExp "flat" $ F.BasicOp $ F.Replicate (flatShape []) $ argSExp xs
    flatten ds = do
      fxs <- argVar "xs" xs
      F.letExp "flat" $
        F.BasicOp $
          F.Reshape fxs $
            F.NewShape [F.DimSplice 0 (length ds) $ flatShape ds] $
              flatShape ds

intrinsic :: Text -> Translate
intrinsic "head" [Arg xs] t =
  mapValues headValue xs t
  where
    headValue fxs _ = do
      txs <- F.lookupType fxs
      F.letSubExp "head" $
        F.BasicOp $
          F.Index fxs $
            F.fullSlice txs [F.DimFix $ constInt64 0]
intrinsic "tail" [Arg xs] t =
  mapValues tailValue xs t
  where
    tailValue fxs rt = do
      txs <- F.lookupType fxs
      F.letSubExp "tail" $
        F.BasicOp $
          F.Index fxs $
            F.fullSlice txs [F.DimSlice (constInt64 1) (F.arraySize 0 rt) (constInt64 1)]
intrinsic "length" [Arg xs] _ = do
  ts <- compileArrayTypes $ argType xs
  case ts of
    t : _ -> pure [F.arraySize 0 t]
    [] -> error "Futhark.Intrinsics: length of a value of no type"
intrinsic "append" [Arg xs, Arg ys] t = do
  rts <- compileArrayTypes t
  fxs <- argVars "xs" xs
  fys <- argVars "ys" ys
  sequence
    [ F.letSubExp "append" $ F.BasicOp $ F.Concat 0 (fx NE.:| [fy]) (F.arraySize 0 rt)
    | (fx, fy, rt) <- zip3 fxs fys rts
    ]
intrinsic "reverse" [Arg xs] t =
  mapValues (\fxs _ -> F.Var <$> compileReverse fxs) xs t
intrinsic "reduce" [FunArg op, Arg xs] _ = do
  fxs <- argVar "xs" xs
  txs <- F.lookupType fxs
  first <- F.letExp "first" =<< F.eIndex fxs [F.eSubExp $ constInt64 0]
  n <-
    F.letSubExp "n"
      =<< F.eBinOp
        (F.Sub F.Int64 F.OverflowWrap)
        (F.eSubExp $ F.arraySize 0 txs)
        (F.eSubExp $ constInt64 1)
  rest <-
    F.letExp "rest" $
      F.BasicOp $
        F.Index fxs $
          F.fullSlice txs [F.DimSlice (constInt64 1) n (constInt64 1)]
  singleton <$> compileFold "reduce" op first rest
intrinsic "reduce/zero" [FunArg op, Arg zero, Arg xs] _ =
  singleton <$> compileReduce (F.Reduce F.Noncommutative op [argSExp zero]) xs
intrinsic "sum" [Arg xs] _ = do
  lam <- F.binOpLambda (F.Add F.Int64 F.OverflowWrap) (F.IntType F.Int64)
  fmap singleton $
    compileReduce (F.Reduce F.Commutative lam [constInt64 0]) =<< compileFlatten xs
intrinsic "flatten" [Arg xs] t =
  fmap singleton $ reshapeTo "flatten" t =<< argVar "xs" xs
intrinsic "reshape" [Arg xs] t =
  fmap singleton $ reshapeTo "reshape" t =<< argVar "xs" xs
intrinsic "transpose2d" [Arg xs] _ = do
  fxs <- argVar "xs" xs
  fmap singleton $ F.letSubExp "transpose" $ F.BasicOp $ F.Rearrange fxs [1, 0]
intrinsic "index" [Arg xs, Arg i] t =
  mapValues indexValue xs t
  where
    indexValue fxs _ =
      F.letSubExp "index" =<< F.eIndex fxs [F.eSubExp $ argSExp i]
intrinsic "index2d" [Arg xs, Arg is] t = do
  fis <- argVar "is" is
  mapValues (indexValue fis) xs t
  where
    indexValue fis fxs _ =
      F.letSubExp "index" =<< F.eIndex fxs [at fis 0, at fis 1]
    at v n = F.eIndex v [F.eSubExp $ constInt64 n]
intrinsic "fold" [FunArg op, Arg acc, Arg xs] _ = do
  facc <- argVar "acc" acc
  fxs <- argVar "xs" xs
  singleton <$> compileFold "fold" op facc fxs
intrinsic "fold-right" [FunArg op, Arg acc, Arg xs] _ = do
  facc <- argVar "acc" acc
  fxs <- argVar "xs" xs
  reversed <- compileReverse fxs
  singleton <$> compileFold "fold-right" (swapParams op) facc reversed
  where
    swapParams lam =
      case F.lambdaParams lam of
        [x, y] -> lam {F.lambdaParams = [y, x]}
        ps -> error $ "fold-right: operator takes " ++ show (length ps) ++ " parameters"
intrinsic "trace" [Arg _, Arg x] _ = pure $ argSExps x
intrinsic "trace-file" [Arg _, Arg _, Arg x] _ = pure $ argSExps x
intrinsic "iota/static" [] t =
  fmap singleton $
    reshapeTo "iota" t
      =<< F.letExp "iota" (F.iota64 $ constInt64 $ product $ intShape $ arrayTypeShape t)
intrinsic "undefined" [] t = do
  failure <-
    F.letExp "undefined" $
      F.BasicOp $
        F.Assert (F.Constant $ F.BoolValue False) $
          F.ErrorMsg ["undefined"]
  blanks <- mapM (F.letSubExp "blank" <=< F.eBlank) =<< compileArrayTypes t
  F.certifying (F.Certs [failure]) $
    mapM (F.letSubExp "undefined" . F.BasicOp . F.Opaque F.OpaqueNil) blanks
intrinsic "iota" _ _ = unsupported "iota"
intrinsic "read-file" _ _ = unsupported "read-file"
intrinsic "read-file-f32bin" _ _ = unsupported "read-file-f32bin"
intrinsic "reify-dim" _ _ = unsupported "reify-dim"
intrinsic "reify-shape" _ _ = unsupported "reify-shape"
intrinsic name as _ =
  error $
    "Futhark.Intrinsics: no translation for "
      ++ T.unpack name
      ++ " applied to "
      ++ show (length as)
      ++ " arguments"

unsupported :: Text -> FutharkM a
unsupported name =
  error $ "Futhark.Intrinsics: unsupported " ++ T.unpack name
