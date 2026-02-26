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
    (\\),
  )
where

import Control.Monad
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
import Data.SBV.Dynamic (generateSMTBenchmarkProof)
import GHC.IsList
import Prettyprinter
import Shape
import System.IO.Unsafe
import Util

type SShape = SList Integer
type SDim = SInteger

data SEnv v
  = SEnv
  { senvMap :: Map v (Either SDim SShape)
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

lookupSymShp :: (MonadSymbolic v m) => v -> m SShape
lookupSymShp v = do
  msym <- gets $ (M.!? v) . senvMap
  case msym of
    Nothing -> do
      s <- symbolic $ prettyString v
      constrain $ SL.all (.>= literal 0) s
      modify $ \senv -> senv {senvMap = M.insert v (Right s) $ senvMap senv}
      pure s
    Just (Right s) -> pure s
    Just (Left _) -> error "got dim when looking up sym shp"

lookupSymDim :: (MonadSymbolic v m) => v -> m SDim
lookupSymDim v = do
  msym <- gets $ (M.!? v) . senvMap
  case msym of
    Nothing -> do
      s <- symbolic $ prettyString v
      constrain $ s .>= literal 0
      modify $ \senv -> senv { senvMap = M.insert v (Left s) $ senvMap senv }
      pure s
    Just (Left s) -> pure s
    Just (Right _) -> error "got shp when looking up sym dim"

toSShape :: (MonadSymbolic v m) => Shape v -> m SShape
toSShape (ShapeVar v) = lookupSymShp v
toSShape (ShapeDim d) = do
  symD <- symDim d
  pure $ SL.singleton symD
  where
    symDim (DimVar v) = lookupSymDim v
    symDim (DimN n) = pure $ literal n
    symDim (Add ds) = do
      symDs <- mapM symDim ds
      pure $ sum symDs
    symDim (Mul ds) = do
      symDs <- mapM symDim ds
      pure $ product symDs
    symDim (Sub []) = pure $ literal 0
    symDim (Sub (d:ds)) = do
      symD <- symDim d
      symDs <- mapM symDim ds
      pure $ symD - sum symDs
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
  let (ThmResult res) = unsafePerformIO $ do
        prove $ op <$> toSShape s <*> toSShape t in
  case res of
    Unsatisfiable _ _ -> True
    _else -> False

(@=) :: (Ord v, Pretty v) => Shape v -> Shape v -> Bool
(@=) = askShapes (.==)

maximumShape :: (Ord v, Pretty v, Foldable t) => t (Shape v) -> Maybe (Shape v)
maximumShape =
  foldM
    ( \next shape ->
        if askShapes SL.isPrefixOf shape next
          then pure next
          else
            if askShapes SL.isPrefixOf next shape
              then pure shape
              else mempty
    )
    mempty

(\\) :: (Eq v, Ord v, Show v, Pretty v) => Shape v -> Shape v -> Maybe (Shape v)
s \\ t
  | s @= t = Just mempty
Concat [] \\ _ = Nothing
s \\ Concat [] = pure s
(Concat ss) \\ (Concat ts)
  | last ss @= last ts = Concat (init ss) \\ Concat (init ts)
(Concat ss) \\ t
  | last ss @= t = pure $ Concat $ init ss
s \\ t = error $ unlines ["No compatible p frame", prettyString s, prettyString t]
