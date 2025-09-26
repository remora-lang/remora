module RemoraPrelude (prelude) where

import Control.Monad
import Data.Text (Text)
import Interpreter.Value
import Syntax
import VName

prelude :: (Monad m) => [(Text, Type Text, Val m)]
prelude =
  [ -- ( "head",
    --  Forall
    --    [("t", KindAtom)]
    --    ( DProd
    --        [("d", SortDim)]
    --        ( DProd
    --            [("s", SortShape)]
    --            ( [TArr (TVar "t") (Shape [Add [Dim 1, IdxVar "d"], IdxVar "s"])]
    --                :-> TArr (TVar "t") (IdxVar "s")
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
