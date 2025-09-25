module CLI.REPL (repl) where

import System.Console.Haskeline

repl :: IO ()
repl = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine ">> "
      case minput of
        Nothing -> return ()
        Just "quit" -> return ()
        Just input -> do
          outputStrLn $ "Input was: " ++ input
          loop
