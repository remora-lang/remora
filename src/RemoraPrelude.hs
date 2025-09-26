module RemoraPrelude (prelude) where

import Control.Monad
import Data.Text (Text)
import Interpreter.Value
import Syntax
import VName

prelude :: (Monad m) => [(Text, Type Text, Val m)]
prelude =
  [ ( "length",
      Forall
        [("t", KindAtom)]
        ( DProd
            [("d", SortDim), ("s", SortShape)]
            ( [TArr (TVar "t") (Concat [Shape [ShapeVar "d"], ShapeVar "s"])]
                :-> TArr Int mempty
            )
        ),
      undefined
      -- ValFun $ \[xs] ->
      --  case xs of
      --    [ValArray shape _] ->
      --      -- TODO: shape must be decremented
      --      pure $ ValArray shape vs
      --    _ -> error $ "head: " <> show xs
    ),
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
    ( "+",
      [Int, Int] :-> Int,
      ValFun $ \[ValBase (IntVal x), ValBase (IntVal y)] ->
        pure $ ValBase $ IntVal $ x + y
    )
  ]
