module CLI.Futhark
  ( FutharkBackend (..),
    Input (..),
    compileBackend,
    compileAndRun,
    futharkInput,
    futharkValue,
  )
where

import Control.Concurrent.Async (Concurrently (..), runConcurrently)
import Control.Exception (IOException, catch, throwIO, try)
import Control.Monad (unless)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Data (Data, Typeable)
import Data.Foldable (for_)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import Data.Vector.Storable qualified as SVec
import Futhark.Data qualified as F
import Futhark.Data.Reader qualified as F
import Interpreter qualified
import Interpreter.Value
import Syntax (Base (..))
import System.Exit (ExitCode (..))
import System.FilePath ((<.>), (</>))
import System.IO (IOMode (ReadMode), hClose, withBinaryFile)
import System.IO.Error (isResourceVanishedError)
import System.IO.Temp (withSystemTempDirectory)
import System.Process
  ( CreateProcess (..),
    StdStream (..),
    proc,
    waitForProcess,
    withCreateProcess,
  )
import Util

data FutharkBackend
  = C
  | CUDA
  deriving (Data, Typeable, Show, Eq)

backendOptions :: FutharkBackend -> [String]
backendOptions C = ["--seq-mem", "--backend=c"]
backendOptions CUDA = ["--gpu-mem", "--backend=cuda"]

data Input
  = InputValues [Interpreter.Val]
  | InputFile FilePath

compileAndRun ::
  FutharkBackend ->
  String ->
  Text ->
  Text ->
  Input ->
  IO (Either Error Interpreter.Val)
compileAndRun backend name ir entry input =
  withSystemTempDirectory "remora" $ \dir -> do
    compiled <- compileBackend backend dir name ir
    case compiled of
      Left err -> pure $ Left err
      Right _ ->
        (futharkOutput =<<) <$> runProgram (dir </> name) ["-e", T.unpack entry] input

compileBackend :: FutharkBackend -> FilePath -> String -> Text -> IO (Either Error Text)
compileBackend backend dir name ir = do
  let source = dir </> name <.> "fut_soacs"
  T.writeFile source ir
  fmap T.decodeUtf8Lenient
    <$> runProgram "futhark" (["dev"] <> backendOptions backend <> [source]) (InputValues mempty)

runProgram :: FilePath -> [String] -> Input -> IO (Either Error BS.ByteString)
runProgram program arguments input = do
  result <- try $ case input of
    InputFile file -> withBinaryFile file ReadMode $ \handle -> run (UseHandle handle) mempty
    InputValues values ->
      either (pure . Left) (run CreatePipe . T.encodeUtf8) $ futharkInput values
  pure $ case result of
    Left err -> Left $ T.pack $ show (err :: IOException)
    Right outcome -> outcome
  where
    run stream bytes =
      withCreateProcess
        (proc program arguments)
          { std_in = stream,
            std_out = CreatePipe,
            std_err = CreatePipe
          }
        $ \mstdin mstdout mstderr process -> do
          (_, out, err) <-
            runConcurrently $
              (,,)
                <$> Concurrently (for_ mstdin $ \handle -> write handle bytes)
                <*> Concurrently (readAll mstdout)
                <*> Concurrently (readAll mstderr)
          code <- waitForProcess process
          pure $ case code of
            ExitSuccess -> Right out
            ExitFailure _
              | BS.null err -> Left $ T.decodeUtf8Lenient out
              | otherwise -> Left $ T.decodeUtf8Lenient err

    write handle bytes =
      (BS.hPut handle bytes >> hClose handle) `catch` \err ->
        unless (isResourceVanishedError err) $ throwIO err

    readAll = maybe (pure mempty) BS.hGetContents

futharkInput :: [Interpreter.Val] -> Either Error Text
futharkInput = fmap T.unlines . mapM value
  where
    value (ValBase b) = base b
    value (ValArray [] [v]) = value v
    value (ValArray [_] vs) = list <$> mapM value vs
    value (ValArray (_ : ds) vs) =
      list <$> mapM (value . ValArray ds) (chunksOf (product ds) vs)
    value v = Left $ "cannot pass as input: " <> prettyText v

    list vs = "[" <> T.intercalate ", " vs <> "]"

    base (IntVal x) = Right $ prettyText x <> "i64"
    base (FloatVal x) = Right $ T.pack (show x) <> "f32"
    base (BoolVal True) = Right "true"
    base (BoolVal False) = Right "false"

futharkOutput :: BS.ByteString -> Either Error Interpreter.Val
futharkOutput out =
  case F.readValues $ LBS.fromStrict out of
    Just [value] -> futharkValue value
    Just values ->
      Left $ "expected one result value, but got " <> prettyText (length values)
    Nothing ->
      Left $ T.unlines ["cannot read program output:", T.decodeUtf8Lenient out]

futharkValue :: F.Value -> Either Error Interpreter.Val
futharkValue (F.I64Value shape vec) =
  Right $ toVal shape $ map (IntVal . fromIntegral) $ SVec.toList vec
futharkValue (F.F32Value shape vec) =
  Right $ toVal shape $ map FloatVal $ SVec.toList vec
futharkValue (F.BoolValue shape vec) =
  Right $ toVal shape $ map BoolVal $ SVec.toList vec
futharkValue value =
  Left $ "unsupported result type: " <> F.valueTypeText (F.valueType value)

toVal :: F.Vector Int -> [Base] -> Interpreter.Val
toVal shape = ValArray (SVec.toList shape) . map ValBase
