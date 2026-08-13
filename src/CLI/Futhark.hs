module CLI.Futhark
  ( FutharkBackend (..),
    backendOptions,
    compileAndRun,
  )
where

import Control.Exception (IOException, try)
import Data.ByteString.Lazy qualified as LBS
import Data.Data (Data, Typeable)
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
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import Util

data FutharkBackend
  = C
  | CUDA
  deriving (Data, Typeable, Show, Eq)

backendOptions :: FutharkBackend -> [String]
backendOptions C = ["--seq-mem", "--backend=c"]
backendOptions CUDA = ["--gpu-mem", "--backend=cuda"]

compileAndRun ::
  FutharkBackend ->
  String ->
  Text ->
  IO (Either Error Interpreter.Val)
compileAndRun backend name ir =
  withSystemTempDirectory "remora" $ \dir -> do
    let source = dir </> name <.> "fut_soacs"
    T.writeFile source ir
    compiled <- run "futhark" $ ["dev"] <> backendOptions backend <> [source]
    case compiled of
      Left err -> pure $ Left err
      Right _ -> (futharkOutput =<<) <$> run (dir </> name) []
  where
    run program arguments = do
      result <- try $ readProcessWithExitCode program arguments ""
      pure $ case result of
        Left err -> Left $ T.pack $ show (err :: IOException)
        Right (ExitSuccess, out, _) -> Right $ T.pack out
        Right (ExitFailure _, out, err) ->
          Left $ T.pack $ if null err then out else err

futharkOutput :: Text -> Either Error Interpreter.Val
futharkOutput out =
  case F.readValues $ LBS.fromStrict $ T.encodeUtf8 out of
    Just [value] -> futharkValue value
    Just values ->
      Left $ "expected one result value, but got " <> prettyText (length values)
    Nothing -> Left $ "cannot read program output:\n" <> out

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
