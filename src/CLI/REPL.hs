module CLI.REPL (repl) where

import Data.Text qualified as T
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
          let mexp = parse "" (T.pack input)
          case mexp of
            Left err -> outputStrLn $ "parse error:\n" <> err
            Right e ->
              case check e of
                Left err -> outputStrLn $ "typecheck error:\n" <> T.unpack err
                Right (prelude, e') ->
                  case interpret prelude e' of
                    Left err -> outputStrLn $ "interpreter error:\n" <> T.unpack err
                    Right v -> outputStrLn $ prettyString v
          loop
