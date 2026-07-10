{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Aeson JSON serialization for the Syntax AST.


module Serializer () where

import Data.Aeson hiding (Array)
import Data.Text qualified as T (Text)
import Syntax qualified
import VName qualified

-- ---------------------------------------------------------------------------
-- Tag
-- ---------------------------------------------------------------------------
instance ToJSON VName.Tag where
  toJSON (VName.Tag tag) = object ["tag" .= ("Tag"   :: T.Text), "getTag" .= tag]

-- ---------------------------------------------------------------------------
-- VName
-- ---------------------------------------------------------------------------
instance ToJSON VName.VName where
  toJSON (VName.VName name tag) = object ["tag" .= ("VName"   :: T.Text), "varName" .= name, "varTag" .= tag]

-- ---------------------------------------------------------------------------
-- Dim
-- ---------------------------------------------------------------------------
instance ToJSON v => ToJSON (Syntax.Dim v) where
  toJSON (Syntax.DimVar v) = object ["tag" .= ("DimVar" :: T.Text), "name" .= v]
  toJSON (Syntax.DimN   n) = object ["tag" .= ("DimN"   :: T.Text), "val"  .= n]
  toJSON (Syntax.Add  ds)  = object ["tag" .= ("Add"    :: T.Text), "dims" .= ds]
  toJSON (Syntax.Mul  ds)  = object ["tag" .= ("Mul"    :: T.Text), "dims" .= ds]
  toJSON (Syntax.Sub  ds)  = object ["tag" .= ("Sub"    :: T.Text), "dims" .= ds]

-- ---------------------------------------------------------------------------
-- Shape
-- ---------------------------------------------------------------------------
instance ToJSON v => ToJSON (Syntax.Shape v) where
  toJSON (Syntax.ShapeVar v)    = object ["tag" .= ("ShapeVar" :: T.Text), "name"   .= v]
  toJSON (Syntax.ShapeDim d)    = object ["tag" .= ("ShapeDim" :: T.Text), "dim"    .= d]
  toJSON (Syntax.Concat  shapes) = object ["tag" .= ("Concat"  :: T.Text), "shapes" .= shapes]

-- ---------------------------------------------------------------------------
-- ISpace
-- ---------------------------------------------------------------------------
instance ToJSON v => ToJSON (Syntax.ISpace v) where
  toJSON (Syntax.Dim   d) = object ["tag" .= ("Dim"   :: T.Text), "dim"   .= d]
  toJSON (Syntax.Shape s) = object ["tag" .= ("Shape" :: T.Text), "shape" .= s]

-- ---------------------------------------------------------------------------
-- ISpaceParam
-- ---------------------------------------------------------------------------
instance ToJSON v => ToJSON (Syntax.ISpaceParam v) where
  toJSON (Syntax.DimParam   v) = object ["tag" .= ("DimParam"   :: T.Text), "name" .= v]
  toJSON (Syntax.ShapeParam v) = object ["tag" .= ("ShapeParam" :: T.Text), "name" .= v]

-- ---------------------------------------------------------------------------
-- Info
-- ---------------------------------------------------------------------------
class InfoToJSON f where
  infoToJSON :: ToJSON a => f a -> Value
instance InfoToJSON Syntax.NoInfo where
  infoToJSON (Syntax.NoInfo) = Null
instance InfoToJSON Syntax.Info where
  infoToJSON (Syntax.Info info) = toJSON info

-- ---------------------------------------------------------------------------
-- TypeParamExp
-- ---------------------------------------------------------------------------
instance ToJSON v => ToJSON (Syntax.TypeParamExp v) where
  toJSON (Syntax.TEAtomTypeParam  v) = object ["tag" .= ("TEAtomTypeParam"  :: T.Text), "name" .= v]
  toJSON (Syntax.TEArrayTypeParam v) = object ["tag" .= ("TEArrayTypeParam" :: T.Text), "name" .= v]

-- ---------------------------------------------------------------------------
-- TypeExp
-- ---------------------------------------------------------------------------
instance ToJSON v => ToJSON (Syntax.TypeExp v) where
  toJSON (Syntax.TEAtomVar  v _)    = object ["tag" .= ("TEAtomVar"  :: T.Text), "name"    .= v]
  toJSON (Syntax.TEArrayVar v _)    = object ["tag" .= ("TEArrayVar" :: T.Text), "name"    .= v]
  toJSON (Syntax.TEBool       _)    = object ["tag" .= ("TEBool"     :: T.Text)]
  toJSON (Syntax.TEInt        _)    = object ["tag" .= ("TEInt"      :: T.Text)]
  toJSON (Syntax.TEFloat      _)    = object ["tag" .= ("TEFloat"    :: T.Text)]
  toJSON (Syntax.TEArray  t s _)    = object ["tag" .= ("TEArray"    :: T.Text), "elem"   .= t,  "shape"  .= s]
  toJSON (Syntax.TEArrow  ts t _)   = object ["tag" .= ("TEArrow"    :: T.Text), "in"     .= ts, "out"    .= t]
  toJSON (Syntax.TEForall ps t _)   = object ["tag" .= ("TEForall"   :: T.Text), "params" .= ps, "body"   .= t]
  toJSON (Syntax.TEPi     ps t _)   = object ["tag" .= ("TEPi"       :: T.Text), "params" .= ps, "body"   .= t]
  toJSON (Syntax.TESigma  ps t _)   = object ["tag" .= ("TESigma"    :: T.Text), "params" .= ps, "body"   .= t]

-- ---------------------------------------------------------------------------
-- TypeParam
-- ---------------------------------------------------------------------------
instance ToJSON v => ToJSON (Syntax.TypeParam v) where
  toJSON (Syntax.AtomTypeParam v) = object ["tag" .= ("AtomTypeParam"  :: T.Text), "name" .= v]

-- ---------------------------------------------------------------------------
-- AtomType
-- ---------------------------------------------------------------------------
instance ToJSON v => ToJSON (Syntax.AtomType v) where
  toJSON (Syntax.AtomTypeVar v)   = object ["tag" .= ("AtomTypeVar"  :: T.Text), "name" .= v]
  toJSON (Syntax.Bool)            = object ["tag" .= ("Bool"         :: T.Text)]
  toJSON (Syntax.Int)             = object ["tag" .= ("Int"          :: T.Text)]
  toJSON (Syntax.Float)           = object ["tag" .= ("Float"        :: T.Text)]
  toJSON ((Syntax.:->) i o)       = object ["tag" .= ("(:->)"        :: T.Text), "in"    .= i, "out"  .= o]
  toJSON (Syntax.Forall p t)      = object ["tag" .= ("Forall"       :: T.Text), "param" .= p, "body" .= t]
  toJSON (Syntax.Pi p t)          = object ["tag" .= ("Pi"           :: T.Text), "param" .= p, "body" .= t]
  toJSON (Syntax.Sigma p t)       = object ["tag" .= ("Sigma"        :: T.Text), "param" .= p, "body" .= t]

-- ---------------------------------------------------------------------------
-- ArrayType
-- ---------------------------------------------------------------------------
instance ToJSON v => ToJSON (Syntax.ArrayType v) where
  toJSON ((Syntax.:@) a s) = object ["tag" .= ("A"   :: T.Text), "arrayTypeAtom" .= a, "arrayTypeShape" .= s]

-- ---------------------------------------------------------------------------
-- Type
-- ---------------------------------------------------------------------------
instance ToJSON v => ToJSON (Syntax.Type v) where
  toJSON (Syntax.AtomType  t)   = object ["tag" .= ("AtomType"   :: T.Text), "type" .= t]
  toJSON (Syntax.ArrayType t)   = object ["tag" .= ("ArrayType"  :: T.Text), "type" .= t]

-- ---------------------------------------------------------------------------
-- Base
-- ---------------------------------------------------------------------------
instance ToJSON Syntax.Base where
  toJSON (Syntax.BoolVal b)  = object ["tag" .= ("BoolVal"  :: T.Text), "lit" .= b]
  toJSON (Syntax.IntVal  n)  = object ["tag" .= ("IntVal"   :: T.Text), "lit" .= n]
  toJSON (Syntax.FloatVal f) = object ["tag" .= ("FloatVal" :: T.Text), "lit" .= f]

-- ---------------------------------------------------------------------------
-- PatBase
-- ---------------------------------------------------------------------------
instance (ToJSON v, InfoToJSON f) => ToJSON (Syntax.PatBase f v) where
  toJSON (Syntax.PatId v t info _) = 
    object ["tag" .= ("PatId"  :: T.Text), "var" .= v, "type" .= t, "info" .= infoToJSON info]

-- ---------------------------------------------------------------------------
-- AtomBase
-- ---------------------------------------------------------------------------
instance (ToJSON (tp v), InfoToJSON f, ToJSON v) => ToJSON (Syntax.AtomBase tp f v) where
  toJSON (Syntax.Base b info _) =
    object ["tag" .= ("Base" :: T.Text), "lit" .= b, "info" .= infoToJSON info]
  toJSON (Syntax.Lambda param body info _) =
    object ["tag" .= ("Lambda"  :: T.Text), "param"  .= param, "body" .= body, "info" .= infoToJSON info]
  toJSON (Syntax.TLambda param body info _) =
    object ["tag" .= ("TLambda" :: T.Text), "param"  .= param, "body" .= body, "info" .= infoToJSON info]
  toJSON (Syntax.ILambda param body info _) =
    object ["tag" .= ("ILambda" :: T.Text), "param"  .= param, "body" .= body, "info" .= infoToJSON info]
  toJSON (Syntax.Box ispace body t info _) =
    object ["tag" .= ("Box"     :: T.Text), "ispace" .= ispace, "array" .= body, "type" .= t, "info" .= infoToJSON info]

-- ---------------------------------------------------------------------------
-- BindBase
-- ---------------------------------------------------------------------------
instance (ToJSON (tp v), InfoToJSON f, ToJSON v) => ToJSON (Syntax.BindBase tp f v) where
  toJSON (Syntax.BindVal v mt body _) =
    object ["tag" .= ("BindVal"    :: T.Text), "var" .= v, "type" .= mt, "expr" .= body]
  toJSON (Syntax.BindType tp ty info _) =
    object ["tag" .= ("BindType"   :: T.Text), "var" .= tp, "type" .= ty, "info" .= infoToJSON info]
  toJSON (Syntax.BindISpace ep ispace _) =
    object ["tag" .= ("BindISpace" :: T.Text), "var" .= ep, "ispace" .= ispace]
  toJSON (Syntax.BindFun v ps mt body info _) =
    object ["tag" .= ("BindFun"    :: T.Text), "var" .= v, "params" .= ps, "type" .= mt, "expr" .= body, "info" .= infoToJSON info]
  toJSON (Syntax.BindTFun v ps mt body info _) =
    object ["tag" .= ("BindTFun"   :: T.Text), "var" .= v, "params" .= ps, "type" .= mt, "expr" .= body, "info" .= infoToJSON info]
  toJSON (Syntax.BindIFun v ps mt body info _) =
    object ["tag" .= ("BindIFun"   :: T.Text), "var" .= v, "params" .= ps, "type" .= mt, "expr" .= body, "info" .= infoToJSON info]

-- ---------------------------------------------------------------------------
-- ExpBase
-- ---------------------------------------------------------------------------
instance (ToJSON (tp v), InfoToJSON f, ToJSON v) => ToJSON (Syntax.ExpBase tp f v) where
  toJSON (Syntax.Var v info _) =
    object ["tag" .= ("Var" :: T.Text), "name" .= v, "info" .= infoToJSON info]
  toJSON (Syntax.Array shape as info _) =
    object ["tag" .= ("Array"      :: T.Text), "dims" .= shape, "atoms"    .= as, "info" .= infoToJSON info]
  toJSON (Syntax.EmptyArray shape t info _) =
    object ["tag" .= ("EmptyArray" :: T.Text), "dims" .= shape, "type" .= t, "info" .= infoToJSON info]
  toJSON (Syntax.Frame shape es info _) =
    object ["tag" .= ("Frame"      :: T.Text), "dims" .= shape, "exprs"    .= es, "info" .= infoToJSON info]
  toJSON (Syntax.EmptyFrame shape t info _) =
    object ["tag" .= ("EmptyFrame" :: T.Text), "dims" .= shape, "type" .= t, "info" .= infoToJSON info]
  toJSON (Syntax.App f arg info _) =
    object ["tag" .= ("App"  :: T.Text), "fun" .= f, "arg"    .= arg, "info" .= infoToJSON info]
  toJSON (Syntax.TApp f t info _) =
    object ["tag" .= ("TApp" :: T.Text), "fun" .= f, "arg"   .= t, "info" .= infoToJSON info]
  toJSON (Syntax.IApp f ispace info _) =
    object ["tag" .= ("IApp" :: T.Text), "fun" .= f, "arg" .= ispace, "info" .= infoToJSON info]
  toJSON (Syntax.Unbox ispace v src body info _) =
    object ["tag" .= ("Unbox" :: T.Text), "ispace" .= ispace, "var" .= v, "target" .= src, "body" .= body, "info" .= infoToJSON info]
  toJSON (Syntax.Let binds body info _) =
    object ["tag" .= ("Let"   :: T.Text), "binds" .= binds, "body" .= body, "info" .= infoToJSON info]

-- ---------------------------------------------------------------------------
-- DeclBase
-- ---------------------------------------------------------------------------
instance (ToJSON (tp v), InfoToJSON f, ToJSON v) => ToJSON (Syntax.DeclBase tp f v) where
  toJSON (Syntax.Def bind) = 
    object ["tag" .= ("Def"   :: T.Text), "bind" .= bind]
  toJSON (Syntax.Entry v args mt body info _) =
    object ["tag" .= ("Entry" :: T.Text), "var" .= v, "params" .= args, "type" .= mt, "expr" .= body, "info" .= infoToJSON info]

-- ---------------------------------------------------------------------------
-- Import
-- ---------------------------------------------------------------------------
instance ToJSON Syntax.Import where
  toJSON (Syntax.Import fp _) = object ["tag" .= ("Import"  :: T.Text), "file" .= fp]

-- ---------------------------------------------------------------------------
-- ProgBase
-- ---------------------------------------------------------------------------
instance (ToJSON (tp v), InfoToJSON f, ToJSON v) => ToJSON (Syntax.ProgBase tp f v) where
  toJSON (Syntax.Prog decs) = object ["tag" .= ("Prog"  :: T.Text), "progDecs" .= decs]
