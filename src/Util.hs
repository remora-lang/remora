module Util
  ( prettyText,
    prettyString,
    ifM,
    (^&&),
    (^||),
    andM,
    orM,
    anyM,
    allM,
    asumM,
    unlessM,
    noSrcPos,
  )
where

import Control.Applicative
import Control.Monad
import Data.Text (Text)
import Prettyprinter
import Prettyprinter.Render.String
import Prettyprinter.Render.Text
import Text.Megaparsec.Pos
  ( SourcePos (..),
    mkPos,
  )

prettyText :: (Pretty x) => x -> Text
prettyText = renderStrict . layoutPretty defaultLayoutOptions . pretty

prettyString :: (Pretty x) => x -> String
prettyString = renderString . layoutPretty defaultLayoutOptions . pretty

ifM :: (Monad m) => m Bool -> m a -> m a -> m a
ifM mb mt mf = do
  b <- mb
  if b then mt else mf

(^&&) :: (Monad m) => m Bool -> m Bool -> m Bool
x ^&& y = ifM x y (pure False)

infixr 3 ^&&

(^||) :: (Monad m) => m Bool -> m Bool -> m Bool
x ^|| y = ifM x (pure True) y

infixr 2 ^||

andM :: (Monad m, Foldable t) => t (m Bool) -> m Bool
andM = allM id

orM :: (Monad m, Foldable t) => t (m Bool) -> m Bool
orM = anyM id

anyM :: (Monad m, Foldable t) => (a -> m Bool) -> t a -> m Bool
anyM p = foldr (\a b -> ifM (p a) (pure True) b) (pure False)

allM :: (Monad m, Foldable t) => (a -> m Bool) -> t a -> m Bool
allM p = foldr (\a b -> ifM (p a) b (pure False)) (pure True)

asumM :: (Monad m, Traversable t, Alternative f) => t (m (f a)) -> m (f a)
asumM = (fmap asum) . sequence

unlessM :: (Monad m) => m Bool -> m () -> m ()
unlessM test m = do
  b <- test
  unless b m

noSrcPos :: SourcePos
noSrcPos = SourcePos "<no location>" (mkPos 1) (mkPos 1)
