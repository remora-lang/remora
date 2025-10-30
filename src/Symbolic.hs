{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Symbolic
  ( maximumShape,
    askShapes,
    (@=),
  )
where

import Control.Monad.State.Class
import Control.Monad.Trans
import Control.Monad.Trans.State (StateT, evalStateT)
import Data.Map (Map)
import Data.Map qualified as M
import Data.SBV hiding (MonadSymbolic, Symbolic, free, prove, sat, symbolic)
import Data.SBV qualified as SBV
import Data.SBV.Internals (SolverContext (..))
import Data.SBV.List qualified as SL
import Data.SBV.Trans qualified as SBV.Trans
import GHC.IsList
import Prettyprinter
import Shape
import System.IO.Unsafe
import Util

type SShape = SList Integer

data SEnv v
  = SEnv
  { senvMap :: Map v SShape
  }

initSEnv :: (Ord v) => SEnv v
initSEnv = SEnv mempty

type MonadSymbolic v m =
  ( SBV.MonadSymbolic m,
    MonadState (SEnv v) m,
    Ord v,
    Pretty v,
    SolverContext m,
    MonadIO m
  )

symbolic :: (MonadSymbolic v m, SymVal a) => String -> m (SBV a)
symbolic = SBV.Trans.symbolic

lookupSym :: (MonadSymbolic v m) => v -> m SShape
lookupSym v = do
  msym <- gets $ (M.!? v) . senvMap
  case msym of
    Nothing -> do
      s <- symbolic $ prettyString v
      modify $ \senv -> senv {senvMap = M.insert v s $ senvMap senv}
      pure s
    Just s -> pure s

toSShape :: (MonadSymbolic v m) => Shape v -> m SShape
toSShape (ShapeVar v) = lookupSym v
toSShape (ShapeDim d) = do
  sym_d <- symDim d
  constrain $ SL.length sym_d .== 1
  pure sym_d
  where
    symDim (DimVar v) = lookupSym v
    symDim (DimN n) =
      symInt n
    symDim (Add ds) = do
      sym_ds <- mapM symDim ds
      sym_sum <- symbolic "sum"
      constrain $ SL.head sym_sum .== sum (map SL.head sym_ds)
      pure sym_sum
    symInt =
      pure . SL.singleton . fromInteger . toInteger
toSShape (Concat ss) =
  (SL.concat . fromList) <$> mapM toSShape ss

-- This may not work; stateful actions might need to be performed
-- in isolation of constraint generation.
-- https://github.com/LeventErkok/sbv/blob/7c8640c200c539c9a5e7d46ba705fa11ddd65835/Data/SBV/Utils/ExtractIO.hs#L27
type Symbolic v = StateT (SEnv v) SBV.Symbolic

evalState :: (Ord v) => Symbolic v a -> SBV.Symbolic a
evalState = flip evalStateT initSEnv

prove :: (Ord v) => Symbolic v SBool -> IO ThmResult
prove = SBV.prove . evalState

instance SolverContext (Symbolic v) where
  constrain = lift . constrain
  softConstrain = lift . softConstrain
  namedConstraint s a = lift $ namedConstraint s a
  constrainWithAttribute cs a = lift $ constrainWithAttribute cs a
  setInfo s ss = lift $ setInfo s ss
  setOption = lift . setOption
  setLogic = lift . setLogic
  setTimeOut = lift . setTimeOut
  contextState = lift contextState
  internalVariable = lift . internalVariable

askShapes ::
  (Ord v, Pretty v) =>
  (SShape -> SShape -> SBool) ->
  Shape v ->
  Shape v ->
  Bool
askShapes op s t =
  show
    ( unsafePerformIO $
        prove $
          op <$> toSShape s <*> toSShape t
    )
    == "Q.E.D."

(@=) :: (Ord v, Pretty v) => Shape v -> Shape v -> Bool
(@=) = askShapes (.==)

maximumShape :: (Ord v, Pretty v, Foldable t) => t (Shape v) -> Shape v
maximumShape =
  foldr
    ( \next shape ->
        if askShapes (.<) shape next
          then next
          else shape
    )
    mempty
