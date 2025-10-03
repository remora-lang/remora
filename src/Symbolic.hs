{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Symbolic where

import Control.Monad.State.Class
import Control.Monad.Trans
import Control.Monad.Trans.State (StateT, evalStateT)
import Data.Map (Map)
import Data.Map qualified as M
import Data.SBV hiding (MonadSymbolic, Symbolic, prove, symbolic)
import Data.SBV qualified as SBV
import Data.SBV.Control
import Data.SBV.Internals (SolverContext)
import Data.SBV.List qualified as SL
import Data.SBV.RegExp
import Data.SBV.Trans qualified as SBV.Trans
import Data.SBV.Tuple
import GHC.IsList
import Prettyprinter
import Shape hiding (Shape)
import Shape qualified
import System.IO.Unsafe
import Util
import VName

type SShape = SList Integer

data SEnv
  = SEnv
  { senvMap :: Map VName SShape
  }

initSEnv :: SEnv
initSEnv = SEnv mempty

type MonadSymbolic m =
  ( SBV.MonadSymbolic m,
    MonadState SEnv m,
    SolverContext m,
    MonadIO m
  )

type Shape = Shape.Shape VName

symbolic :: (MonadSymbolic m, SymVal a) => String -> m (SBV a)
symbolic = SBV.Trans.symbolic

lookupSym :: (MonadSymbolic m) => VName -> m SShape
lookupSym v = do
  msym <- gets $ (M.!? v) . senvMap
  case msym of
    Nothing -> do
      sym <- symbolic $ prettyString v
      modify $ \senv -> senv {senvMap = M.insert v sym $ senvMap senv}
      pure sym
    Just sym -> pure sym

toSShape :: (MonadSymbolic m) => Shape -> m SShape
toSShape (ShapeVar v) = lookupSym v
toSShape (ShapeDim d) = do
  sym_d <- symDim d
  constrain $ SL.length sym_d .== 1
  pure sym_d
  where
    symDim (DimVar v) = lookupSym v
    symDim (Dim d) =
      symInt d
    symDim (Add ds) = do
      sym_ds <- mapM symDim ds
      sym_sum <- symbolic "sum"
      constrain $ SL.head sym_sum .== sum (map SL.head sym_ds)
      pure sym_sum
    symInt =
      pure . SL.singleton . fromInteger . toInteger
toSShape (Concat ss) =
  (SL.concat . fromList) <$> mapM toSShape ss

type Symbolic = StateT SEnv SBV.Symbolic

evalState :: Symbolic a -> SBV.Symbolic a
evalState = flip evalStateT initSEnv

runSymbolic :: Symbolic a -> IO a
runSymbolic = runSMT . evalState

-- prove :: (Provable a) => Symbolic a -> IO ThmResult
-- prove = SBV.prove . evalState

-- eqShapes :: Shape -> Shape -> Bool
-- eqShapes s t = unsafePerformIO m
--  where
--    m :: IO Bool
--    m =
--      modelExists
--        <$> ( prove
--                ( (.==)
--                    <$> toSShape s
--                    <*> toSShape t
--                )
--            )
