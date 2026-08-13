{-# LANGUAGE TypeFamilies #-}

module Futhark.Monad
  ( FutharkM,
    runFutharkM,
    Arg (..),
    SExpArg (..),
    ArrayType,
    mkBody,
    assertNoStms,
    addFunction,
    compileVName,
    compileArrayType,
    constInt64,
    intShape,
  )
where

import Control.Monad (unless)
import Control.Monad.Error.Class
import Control.Monad.State
import Control.Monad.Trans.Except
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Futhark.Analysis.PrimExp.Convert qualified as F
import Futhark.Builder (BuilderT, MonadBuilder (..), runBuilderT)
import Futhark.Construct qualified as F
import Futhark.IR.SOACS qualified as F
import Futhark.MonadFreshNames (MonadFreshNames (..), VNameSource (..), newNameSource)
import Syntax hiding (ArrayType, AtomType, Dim, Shape)
import Syntax qualified
import Util
import VName

type Dim = Syntax.Dim VName

type Shape = Syntax.Shape VName

type AtomType = Syntax.AtomType VName

type ArrayType = Syntax.ArrayType VName

data SExpArg = SExpArg
  { argFrame :: [Int],
    argSExp :: F.SubExp,
    argType :: ArrayType
  }
  deriving (Show, Eq)

data Arg
  = Arg SExpArg
  | FunArg (F.Lambda F.SOACS)
  deriving (Show, Eq)

data St = St
  { stateNameSource :: VNameSource,
    stateFuns :: [F.FunDef F.SOACS]
  }

instance MonadFreshNames (StateT St (Except Error)) where
  getNameSource = gets stateNameSource
  putNameSource src = modify $ \s -> s {stateNameSource = src}

newtype FutharkM a = FutharkM (BuilderT F.SOACS (StateT St (Except Error)) a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadError Error,
      MonadFreshNames,
      F.HasScope F.SOACS,
      F.LocalScope F.SOACS
    )

instance MonadBuilder FutharkM where
  type Rep FutharkM = F.SOACS
  mkExpDecM pat e = FutharkM $ mkExpDecM pat e
  mkBodyM stms res = FutharkM $ mkBodyM stms res
  mkLetNamesM pat e = FutharkM $ mkLetNamesM pat e
  addStms = FutharkM . addStms
  collectStms (FutharkM m) = FutharkM $ collectStms m

runFutharkM ::
  Tag ->
  FutharkM a ->
  Either Error (a, F.Stms F.SOACS, [F.FunDef F.SOACS], Tag)
runFutharkM tag (FutharkM m) = do
  ((x, stms), s) <- runExcept $ runStateT (runBuilderT m mempty) initialState
  let VNameSource counter = stateNameSource s
  pure (x, stms, reverse $ stateFuns s, Tag counter)
  where
    initialState = St (newNameSource $ getTag tag) mempty

mkBody :: FutharkM [F.SubExp] -> FutharkM (F.Body F.SOACS)
mkBody = F.buildBody_ . (fmap . fmap) F.subExpRes

assertNoStms :: FutharkM a -> FutharkM a
assertNoStms m = do
  (x, stms) <- collectStms m
  unless (null stms) $
    error $
      unlines ["assertNoStms: emitted statements:", F.prettyString stms]
  pure x

addFunction :: F.FunDef F.SOACS -> FutharkM ()
addFunction fun =
  FutharkM $ lift $ modify $ \s -> s {stateFuns = fun : stateFuns s}

compileVName :: VName -> F.VName
compileVName v =
  F.VName (F.nameFromText $ varName v) (getTag $ varTag v)

constInt64 :: Int -> F.SubExp
constInt64 = F.intConst F.Int64 . fromIntegral

intShape :: Shape -> [Int]
intShape shape =
  fromMaybe (error $ "intShape: not statically known: " ++ prettyString shape) $
    shapeToInts (dimToInt $ const Nothing) (const Nothing) $
      normShape shape

compileAtomType :: AtomType -> FutharkM F.Type
compileAtomType Bool = pure $ F.Prim F.Bool
compileAtomType Int = pure $ F.Prim $ F.IntType F.Int64
compileAtomType Float = pure $ F.Prim $ F.FloatType F.Float32
compileAtomType t =
  error $ unlines ["compileAtomType: unhandled:", prettyString t]

compileDim :: Dim -> FutharkM F.SubExp
compileDim dim = F.toSubExp "dim" =<< dimExp dim
  where
    dimExp :: Dim -> FutharkM (F.TPrimExp Int64 F.VName)
    dimExp (DimN x) = pure $ fromIntegral x
    dimExp (DimVar v) = pure $ F.le64 $ compileVName v
    dimExp (Add ds) = sum <$> mapM dimExp ds
    dimExp (Mul ds) = product <$> mapM dimExp ds
    dimExp (Sub []) = pure 0
    dimExp (Sub [d]) = negate <$> dimExp d
    dimExp (Sub (d : ds)) =
      (\x xs -> x - sum xs) <$> dimExp d <*> mapM dimExp ds

compileShape :: Shape -> FutharkM [F.SubExp]
compileShape = compileNormShape . normShape
  where
    compileNormShape (ShapeDim d) = pure <$> compileDim d
    compileNormShape (Concat ss) = concat <$> mapM compileNormShape ss
    compileNormShape (ShapeVar v) =
      error $ "compileShape: existential shape of unknown rank: " ++ prettyString v

compileArrayType :: ArrayType -> FutharkM F.Type
compileArrayType (t :@ shape) = do
  t' <- compileAtomType t
  F.arrayOfShape t' . F.Shape <$> compileShape shape
