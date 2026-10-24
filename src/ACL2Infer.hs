-- | Inferring omitted type and index applications with ACL2, for the
-- @--acl2-infer@ option of @remora interpret@ and @remora futhark@.
--
-- When a program fails to type check because a polymorphic function is
-- applied without its type or index arguments, the program text is sent over
-- a pipe to ACL2's Remora type checker (the @kestrel/remora/elaborator@ book),
-- which infers the missing applications and prints the elaborated program.
-- The elaborated program is written next to the original, @xxx.remora@
-- becoming @xxx-typed.remora@, and remora then runs on that file.
--
-- ACL2 is started with the command in the @ACL2@ environment variable, or with
-- @acl2@ from the search path. The command may start ACL2 in a container, e.g.
-- @ACL2="docker run --rm -i remora-acl2:v3 acl2"@.
--
-- TODO: check that the elaborated program is the only way of filling in the
-- missing applications. For now it is the one ACL2's inference finds.
module ACL2Infer
  ( isMissingApplication,
    inferWithACL2,
    typedPath,
  )
where

import Control.Concurrent.Async (Concurrently (..), runConcurrently)
import Control.Exception (IOException, try)
import Data.ByteString qualified as BS
import Data.Char (isSpace)
import Data.Foldable (for_)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import System.Directory (findExecutable)
import System.Environment (lookupEnv)
import System.FilePath (dropExtension, takeFileName, (<.>))
import System.IO (hClose)
import System.Process
  ( CreateProcess (..),
    StdStream (..),
    shell,
    waitForProcess,
    withCreateProcess,
  )
import TypeCheck (missingApplication)
import Util (Error)

-- | Whether a type checking error is the one for a missing type or index
-- application.
isMissingApplication :: Error -> Bool
isMissingApplication = T.isInfixOf missingApplication

-- | Where the elaborated program goes: @xxx.remora@ becomes @xxx-typed.remora@.
typedPath :: FilePath -> FilePath
typedPath path = dropExtension path <> "-typed" <.> "remora"

-- | The shell command that starts ACL2, if any.
acl2Command :: IO (Maybe String)
acl2Command = do
  env <- lookupEnv "ACL2"
  case env of
    Just cmd | not (all isSpace cmd) -> pure $ Just cmd
    _ -> fmap (const "acl2") <$> findExecutable "acl2"

-- | Have ACL2 elaborate the program in the given file, and write the result to
-- 'typedPath'. Returns that path, or why there is no elaborated program.
inferWithACL2 :: FilePath -> IO (Either Text FilePath)
inferWithACL2 path = do
  source <- T.readFile path
  mcmd <- acl2Command
  case mcmd of
    Nothing ->
      pure $
        Left
          "no ACL2 found: set ACL2 to the command that starts it, or put acl2 on the search path."
    Just cmd -> do
      result <- try $ runShell cmd $ T.encodeUtf8 $ acl2Input source
      case result of
        Left err ->
          pure $ Left $ "could not run " <> T.pack cmd <> ": " <> T.pack (show (err :: IOException))
        Right out ->
          case section $ T.decodeUtf8Lenient out of
            Just ("elaborated", program) -> do
              let typed = typedPath path
              T.writeFile typed $ header path source <> program
              pure $ Right typed
            Just (kind, details) ->
              pure $
                Left $
                  "ACL2 could not infer them ("
                    <> T.replace "-" " " kind
                    <> "):\n"
                    <> T.strip details
            Nothing ->
              pure $
                Left $
                  "no result from "
                    <> T.pack cmd
                    <> "; does that ACL2 have the book kestrel/remora/elaborator?"

-- | Run a shell command with the given standard input, returning its standard
-- output. (Standard error is read and dropped.) The bytes pass through
-- unchanged, so the program text reaches ACL2 as UTF-8.
runShell :: String -> BS.ByteString -> IO BS.ByteString
runShell cmd input =
  withCreateProcess
    (shell cmd)
      { std_in = CreatePipe,
        std_out = CreatePipe,
        std_err = CreatePipe
      }
    $ \mstdin mstdout mstderr process -> do
      (_, out, _) <-
        runConcurrently $
          (,,)
            <$> Concurrently (for_ mstdin $ \h -> BS.hPut h input >> hClose h)
            <*> Concurrently (maybe (pure mempty) BS.hGetContents mstdout)
            <*> Concurrently (maybe (pure mempty) BS.hGetContents mstderr)
      _ <- waitForProcess process
      pure out

-- | What is sent to ACL2: load the elaborator, elaborate the program, quit.
acl2Input :: Text -> Text
acl2Input source =
  T.unlines
    [ "(include-book \"kestrel/remora/elaborator\" :dir :system)",
      "(remora::elaborate-program-string " <> lispString source <> " state)",
      "(good-bye 0)"
    ]

-- | A Lisp string literal.
lispString :: Text -> Text
lispString s = "\"" <> T.concatMap escape s <> "\""
  where
    escape '"' = "\\\""
    escape '\\' = "\\\\"
    escape c = T.singleton c

-- | The elaborator's result in ACL2's output: the kind of result and the text
-- between its marker lines.
section :: Text -> Maybe (Text, Text)
section out =
  case break isStart $ T.lines out of
    (_, start : rest) ->
      Just
        ( T.dropEnd 3 $ T.drop (T.length marker) start,
          T.unlines $ takeWhile (/= end) rest
        )
    _ -> Nothing
  where
    marker = "<<<remora-acl2 "
    end = "<<<remora-acl2 end>>>"
    isStart l = marker `T.isPrefixOf` l && ">>>" `T.isSuffixOf` l && l /= end

-- | The start of the elaborated file: a note saying where it came from, and
-- the original's leading comment lines (such as test directives), which
-- ACL2's printer does not keep.
header :: FilePath -> Text -> Text
header path source =
  T.unlines $
    ( ";; Generated by remora --acl2-infer from "
        <> T.pack (takeFileName path)
        <> ": ACL2 inferred its type and index applications."
    )
      : takeWhile isHeaderLine (T.lines source)
  where
    isHeaderLine l = T.null (T.strip l) || ";" `T.isPrefixOf` T.stripStart l
