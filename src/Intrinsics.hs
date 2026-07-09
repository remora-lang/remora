module Intrinsics (intrinsics, maxIntrinsicTag) where

import Control.Monad
import Data.Bifunctor
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Primitive (binOps, unOps)
import Syntax
import Uniquify.Monad
import Uniquify.Type
import Util
import VName

intrinsics :: Map VName (ArrayType VName)
intrinsics = fst intrinsics'

maxIntrinsicTag :: Tag
maxIntrinsicTag = snd intrinsics'

intrinsics' :: (Map VName (ArrayType VName), Tag)
intrinsics' =
  runUniquify mempty initTag $
    M.fromList
      <$> forM
        allFuncs
        ( \(v, t) -> do
            vname <- newVName v
            t' <- uniquifyArrayType t
            pure (vname, t')
        )
  where
    allFuncs :: [(Text, ArrayType Text)]
    allFuncs =
      map (first prettyText) unOps
        ++ map (first prettyText) binOps
        ++ [ ( "head",
               scalar $
                 Forall (AtomTypeParam "t") $
                   scalar $
                     piType (NE.fromList [DimParam "d", ShapeParam "s"]) $
                       scalar $
                         A
                           (AtomTypeVar "t")
                           (Concat [ShapeDim (Add [DimN 1, DimVar "d"]), ShapeVar "s"])
                           :-> A (AtomTypeVar "t") (ShapeVar "s")
             ),
             ( "tail",
               scalar $
                 Forall (AtomTypeParam "t") $
                   scalar $
                     piType (NE.fromList [DimParam "d", ShapeParam "s"]) $
                       scalar $
                         A
                           (AtomTypeVar "t")
                           (Concat [ShapeDim (Add [DimN 1, DimVar "d"]), ShapeVar "s"])
                           :-> A (AtomTypeVar "t") (Concat [ShapeDim (DimVar "d"), ShapeVar "s"])
             ),
             ( "length",
               scalar $
                 Forall (AtomTypeParam "t") $
                   scalar $
                     piType (NE.fromList [DimParam "d", ShapeParam "s"]) $
                       scalar $
                         A (AtomTypeVar "t") (Concat [ShapeDim (DimVar "d"), ShapeVar "s"])
                           :-> A Int mempty
             ),
             ( "append",
               scalar $
                 Forall (AtomTypeParam "t") $
                   scalar $
                     piType (NE.fromList [DimParam "m", DimParam "n", ShapeParam "s"]) $
                       let t = AtomTypeVar "t"
                        in scalar $
                             arrowType
                               ( NE.fromList
                                   [ A t (Concat [ShapeDim (DimVar "m"), ShapeVar "s"]),
                                     A t (Concat [ShapeDim (DimVar "n"), ShapeVar "s"])
                                   ]
                               )
                               ( A
                                   t
                                   (Concat [ShapeDim (Add [DimVar "m", DimVar "n"]), ShapeVar "s"])
                               )
             ),
             ( "reverse",
               scalar $
                 Forall (AtomTypeParam "t") $
                   scalar $
                     piType (NE.fromList [DimParam "d", ShapeParam "s"]) $
                       let arr_t =
                             A
                               (AtomTypeVar "t")
                               (Concat [ShapeDim (DimVar "d"), ShapeVar "s"])
                        in scalar $ arr_t :-> arr_t
             ),
             ( "reduce",
               scalar $
                 Forall (AtomTypeParam "t") $
                   scalar $
                     piType (NE.fromList [DimParam "d", ShapeParam "s"]) $
                       let elem_t = A (AtomTypeVar "t") (ShapeVar "s")
                           op_t = scalar $ NE.fromList [elem_t, elem_t] `arrowType` elem_t
                           arg_t =
                             A
                               (AtomTypeVar "t")
                               (ShapeDim (DimN 1 <> DimVar "d") <> ShapeVar "s")
                        in scalar $ NE.fromList [op_t, arg_t] `arrowType` elem_t
             ),
             ( "sum",
               scalar $
                 Pi (ShapeParam "s") $
                   scalar $
                     A Int (ShapeVar "s") :-> A Int mempty
             ),
             ( "flatten",
               scalar $
                 Forall (AtomTypeParam "t") $
                   scalar $
                     piType (NE.fromList [DimParam "m", DimParam "n", ShapeParam "s"]) $
                       scalar $
                         A
                           (AtomTypeVar "t")
                           (Concat [ShapeDim (DimVar "m"), ShapeDim (DimVar "n"), ShapeVar "s"])
                           :-> A
                             (AtomTypeVar "t")
                             (Concat [ShapeDim (Mul [DimVar "m", DimVar "n"]), ShapeVar "s"])
             ),
             ( "reshape",
               scalar $
                 Forall (AtomTypeParam "t") $
                   scalar $
                     piType (NE.fromList [ShapeParam "s1", ShapeParam "s2"]) $
                       scalar $
                         A (AtomTypeVar "t") (ShapeVar "s1")
                           :-> A (AtomTypeVar "t") (ShapeVar "s2")
             ),
             ( "iota",
               scalar $
                 Pi (DimParam "d") $
                   scalar $
                     A Int (ShapeDim (DimVar "d"))
                       :-> scalar (Sigma (ShapeParam "s") (A Int (ShapeVar "s")))
             ),
             ( "iota/static",
               scalar $ Pi (ShapeParam "s") (A Int (ShapeVar "s"))
             ),
             ( "transpose2d",
               scalar $
                 Forall (AtomTypeParam "t") $
                   scalar $
                     piType (NE.fromList [DimParam "m", DimParam "n"]) $
                       scalar $
                         A
                           (AtomTypeVar "t")
                           (Concat [ShapeDim (DimVar "m"), ShapeDim (DimVar "n")])
                           :-> A
                             (AtomTypeVar "t")
                             (Concat [ShapeDim (DimVar "n"), ShapeDim (DimVar "m")])
             ),
             ( "undefined",
               scalar $
                 Forall (AtomTypeParam "t") $
                   scalar $
                     Pi (ShapeParam "s") (A (AtomTypeVar "t") (ShapeVar "s"))
             ),
             ( "index",
               scalar $
                 Forall (AtomTypeParam "t") $
                   scalar $
                     Pi (DimParam "m") $
                       scalar $
                         arrowType
                           ( NE.fromList
                               [ A
                                   (AtomTypeVar "t")
                                   (ShapeDim (DimVar "m")),
                                 scalar Int
                               ]
                           )
                           (A (AtomTypeVar "t") mempty)
             ),
             ( "index2d",
               scalar $
                 Forall (AtomTypeParam "t") $
                   scalar $
                     piType (NE.fromList [DimParam "m", DimParam "n"]) $
                       scalar $
                         arrowType
                           ( NE.fromList
                               [ A
                                   (AtomTypeVar "t")
                                   (Concat [ShapeDim (DimVar "m"), ShapeDim (DimVar "n")]),
                                 A Int (ShapeDim (DimN 2))
                               ]
                           )
                           (A (AtomTypeVar "t") mempty)
             ),
             ( "fold",
               scalar $
                 forallType (NE.fromList [AtomTypeParam "t", AtomTypeParam "t2"]) $
                   scalar $
                     piType (NE.fromList [DimParam "d", ShapeParam "s", ShapeParam "s2"]) $
                       let elem_t = A (AtomTypeVar "t") (ShapeVar "s")
                           acc_t = A (AtomTypeVar "t2") (ShapeVar "s2")
                           op_t = scalar $ NE.fromList [acc_t, elem_t] `arrowType` acc_t
                           arg_t =
                             A
                               (AtomTypeVar "t")
                               (ShapeDim (DimN 1 <> DimVar "d") <> ShapeVar "s")
                        in scalar $ NE.fromList [op_t, acc_t, arg_t] `arrowType` acc_t
             ),
             ( "trace",
               scalar $
                 forallType (NE.fromList [AtomTypeParam "t", AtomTypeParam "r"]) $
                   scalar $
                     piType (NE.fromList [ShapeParam "s", ShapeParam "q"]) $
                       scalar $
                         arrowType
                           ( NE.fromList
                               [ A (AtomTypeVar "t") (ShapeVar "s"),
                                 A (AtomTypeVar "r") (ShapeVar "q")
                               ]
                           )
                           (A (AtomTypeVar "r") (ShapeVar "q"))
             ),
             ( "trace-file",
               scalar $
                 forallType (NE.fromList [AtomTypeParam "t", AtomTypeParam "r"]) $
                   scalar $
                     piType (NE.fromList [DimParam "d", ShapeParam "s", ShapeParam "q"]) $
                       scalar $
                         arrowType
                           ( NE.fromList
                               [ A Int (ShapeDim (DimVar "d")),
                                 A (AtomTypeVar "t") (ShapeVar "s"),
                                 A (AtomTypeVar "r") (ShapeVar "q")
                               ]
                           )
                           (A (AtomTypeVar "r") (ShapeVar "q"))
             ),
             ( "read-file",
               scalar $
                 Forall (AtomTypeParam "t") $
                   scalar $
                     piType (NE.fromList [DimParam "d", ShapeParam "s"]) $
                       scalar $
                         A Int (ShapeDim (DimVar "d"))
                           :-> A (AtomTypeVar "t") (ShapeVar "s")
             ),
             ( "reify-dim",
               scalar $
                 Pi (DimParam "d") $
                   scalar Int
             ),
             ( "reify-shape",
               scalar $
                 Pi (ShapeParam "s") $
                   scalar $
                     Sigma (DimParam "r") $
                       A Int (ShapeDim $ DimVar "r")
             )
           ]
    scalar = mkScalarArrayType
