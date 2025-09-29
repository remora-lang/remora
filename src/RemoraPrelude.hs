module RemoraPrelude (prelude, Prelude, PreludeDef (..)) where

import Control.Monad
import Data.Text (Text)
import Interpreter.Value
import Syntax
import VName

type Prelude v m = [PreludeDef v m]

data PreludeDef v m
  = PreludeVal v (Type v) (Val m)
  | PreludeType v Kind

prelude :: (Monad m) => Prelude Text m
prelude =
  [ PreludeVal
      "length"
      ( Forall
          [("t", KindAtom)]
          ( DProd
              [("d", SortDim), ("s", SortShape)]
              ( [TArr (TVar "t") (Concat [ShapeDim (DimVar "d"), ShapeVar "s"])]
                  :-> TArr Int mempty
              )
          )
      )
      ( ValTFun $ \[_t] ->
          pure $ ValIFun $ \[_d, _s] ->
            pure $ ValFun $ \[ValArray (d : _) _] ->
              pure $ ValBase $ IntVal $ d
      ),
    PreludeVal
      "+"
      ([Int, Int] :-> Int)
      ( ValFun $ \[ValBase (IntVal x), ValBase (IntVal y)] ->
          pure $ ValBase $ IntVal $ x + y
      )
  ]

-- ( "head",
--  Forall
--    [("t", KindAtom)]
--    ( DProd
--        [("d", SortDim)]
--        ( DProd
--            [("s", SortShape)]
--            ( [TArr (TVar "t") (Shape [Add [Dim 1, ShapeVar "d"], ShapeVar "s"])]
--                :-> TArr (TVar "t") (ShapeVar "s")
--            )
--        )
--    ),
--  ValFun $ \xs ->
--    case xs of
--      [ValArray shape (_ : vs)] ->
--        -- TODO: shape must be decremented
--        pure $ ValArray shape vs
--      _ -> error $ "head: " <> show xs
-- ),
