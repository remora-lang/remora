module CLI.REPL (repl) where

import Control.Monad.IO.Class
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Interpreter
import Parser
import System.Console.Haskeline
import TypeCheck
import Util

repl :: IO ()
repl = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine ">> "
      case minput of
        Nothing -> loop
        Just input -> do
          let m = do
                expr <- parse "" (T.pack input)
                (prelude, expr') <- check expr
                interpret prelude expr'
          case m of
            Left err -> liftIO $ T.putStrLn err
            Right v -> liftIO $ T.putStrLn $ prettyText v
          loop
