{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Symbolic where

import Data.SBV
import Data.SBV.Control
import Data.SBV.List qualified as SL
import Data.SBV.RegExp
import Data.SBV.Tuple
import GHC.IsList
import Prettyprinter
import Shape
import Util

type SShape = SList Integer

toSShape :: (Pretty v) => Shape v -> Symbolic SShape
toSShape (ShapeVar v) = symbolic $ prettyString v
toSShape (ShapeDim d) = do
  sym_d <- symDim d
  constrain $ SL.length sym_d .== 1
  pure sym_d
  where
    symDim (Dim v) =
      symbolic $ prettyString v
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
