module CLI.REPL (repl) where

import Data.Text qualified as T
import Parser
import System.Console.Haskeline
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
          let mexp = parse "" (T.pack input)
          case mexp of
            Left err -> outputStrLn $ "parse error:\n" <> err
            Right e -> outputStrLn $ prettyString e
          loop
