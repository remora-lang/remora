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
  [Interpreter.Val] ->
  IO (Either Error Interpreter.Val)
compileAndRun backend name ir input =
  withSystemTempDirectory "remora" $ \dir -> do
    let source = dir </> name <.> "fut_soacs"
    T.writeFile source ir
    compiled <- run "futhark" (["dev"] <> backendOptions backend <> [source]) mempty
    case compiled of
      Left err -> pure $ Left err
      Right _ ->
        case futharkInput input of
          Left err -> pure $ Left err
          Right stdin -> (futharkOutput =<<) <$> run (dir </> name) [] stdin
  where
    run program arguments stdin = do
      result <- try $ readProcessWithExitCode program arguments $ T.unpack stdin
      pure $ case result of
        Left err -> Left $ T.pack $ show (err :: IOException)
        Right (ExitSuccess, out, _) -> Right $ T.pack out
        Right (ExitFailure _, out, err) ->
          Left $ T.pack $ if null err then out else err

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
