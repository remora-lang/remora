module Intrinsics (intrinsics, maxIntrinsicTag) where

import Control.Monad
import Data.Bifunctor
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
                 forallType [AtomTypeParam "t"] $
                   scalar $
                     piType [DimParam "d", ShapeParam "s"] $
                       scalar $
                         arrowType
                           [ A
                               (AtomTypeVar "t")
                               (Concat [ShapeDim (Add [DimN 1, DimVar "d"]), ShapeVar "s"])
                           ]
                           (A (AtomTypeVar "t") (ShapeVar "s"))
             ),
             ( "tail",
               scalar $
                 forallType [AtomTypeParam "t"] $
                   scalar $
                     piType [DimParam "d", ShapeParam "s"] $
                       scalar $
                         arrowType
                           [ A
                               (AtomTypeVar "t")
                               (Concat [ShapeDim (Add [DimN 1, DimVar "d"]), ShapeVar "s"])
                           ]
                           (A (AtomTypeVar "t") (Concat [ShapeDim (DimVar "d"), ShapeVar "s"]))
             ),
             ( "length",
               scalar $
                 forallType [AtomTypeParam "t"] $
                   scalar $
                     piType [DimParam "d", ShapeParam "s"] $
                       scalar $
                         arrowType
                           [A (AtomTypeVar "t") (Concat [ShapeDim (DimVar "d"), ShapeVar "s"])]
                           (A Int mempty)
             ),
             ( "append",
               scalar $
                 forallType [AtomTypeParam "t"] $
                   scalar $
                     piType [DimParam "m", DimParam "n", ShapeParam "s"] $
                       let t = AtomTypeVar "t"
                        in scalar $
                             arrowType
                               [ A t (Concat [ShapeDim (DimVar "m"), ShapeVar "s"]),
                                 A t (Concat [ShapeDim (DimVar "n"), ShapeVar "s"])
                               ]
                               ( A
                                   t
                                   (Concat [ShapeDim (Add [DimVar "m", DimVar "n"]), ShapeVar "s"])
                               )
             ),
             ( "reverse",
               scalar $
                 forallType [AtomTypeParam "t"] $
                   scalar $
                     piType [DimParam "d", ShapeParam "s"] $
                       let arr_t =
                             A
                               (AtomTypeVar "t")
                               (Concat [ShapeDim (DimVar "d"), ShapeVar "s"])
                        in scalar $ arrowType [arr_t] arr_t
             ),
             ( "reduce",
               scalar $
                 forallType [AtomTypeParam "t"] $
                   scalar $
                     piType [DimParam "d", ShapeParam "s"] $
                       let elem_t = A (AtomTypeVar "t") (ShapeVar "s")
                           op_t = scalar $ arrowType [elem_t, elem_t] elem_t
                           arg_t =
                             A
                               (AtomTypeVar "t")
                               (ShapeDim (DimN 1 <> DimVar "d") <> ShapeVar "s")
                        in scalar $ arrowType [op_t, arg_t] elem_t
             ),
             ( "sum",
               scalar $
                 piType [ShapeParam "s"] $
                   scalar $
                     arrowType [A Int (ShapeVar "s")] (A Int mempty)
             ),
             ( "flatten",
               scalar $
                 forallType [AtomTypeParam "t"] $
                   scalar $
                     piType [DimParam "m", DimParam "n", ShapeParam "s"] $
                       scalar $
                         arrowType
                           [A (AtomTypeVar "t") (Concat [ShapeDim (DimVar "m"), ShapeDim (DimVar "n"), ShapeVar "s"])]
                           (A (AtomTypeVar "t") (Concat [ShapeDim (Mul [DimVar "m", DimVar "n"]), ShapeVar "s"]))
             ),
             ( "reshape",
               scalar $
                 forallType [AtomTypeParam "t"] $
                   scalar $
                     piType [ShapeParam "s1", ShapeParam "s2"] $
                       scalar $
                         arrowType
                           [A (AtomTypeVar "t") (ShapeVar "s1")]
                           (A (AtomTypeVar "t") (ShapeVar "s2"))
             ),
             ( "iota",
               scalar $
                 piType [DimParam "d"] $
                   scalar $
                     arrowType
                       [A Int (ShapeDim (DimVar "d"))]
                       (scalar $ sigmaType [ShapeParam "s"] (A Int (ShapeVar "s")))
             ),
             ( "iota/static",
               scalar $ piType [ShapeParam "s"] (A Int (ShapeVar "s"))
             ),
             ( "transpose2d",
               scalar $
                 forallType [AtomTypeParam "t"] $
                   scalar $
                     piType [DimParam "m", DimParam "n"] $
                       scalar $
                         arrowType
                           [ A
                               (AtomTypeVar "t")
                               (Concat [ShapeDim (DimVar "m"), ShapeDim (DimVar "n")])
                           ]
                           ( A
                               (AtomTypeVar "t")
                               (Concat [ShapeDim (DimVar "n"), ShapeDim (DimVar "m")])
                           )
             ),
             ( "undefined",
               scalar $
                 forallType [AtomTypeParam "t"] $
                   scalar $
                     piType [ShapeParam "s"] (A (AtomTypeVar "t") (ShapeVar "s"))
             ),
             ( "index",
               scalar $
                 forallType [AtomTypeParam "t"] $
                   scalar $
                     piType [DimParam "m"] $
                       scalar $
                         arrowType
                           [ A
                               (AtomTypeVar "t")
                               (ShapeDim (DimVar "m")),
                             scalar Int
                           ]
                           (A (AtomTypeVar "t") mempty)
             ),
             ( "index2d",
               scalar $
                 forallType [AtomTypeParam "t"] $
                   scalar $
                     piType [DimParam "m", DimParam "n"] $
                       scalar $
                         arrowType
                           [ A
                               (AtomTypeVar "t")
                               (Concat [ShapeDim (DimVar "m"), ShapeDim (DimVar "n")]),
                             A Int (ShapeDim (DimN 2))
                           ]
                           (A (AtomTypeVar "t") mempty)
             ),
             ( "fold",
               scalar $
                 forallType [AtomTypeParam "t", AtomTypeParam "t2"] $
                   scalar $
                     piType [DimParam "d", ShapeParam "s", ShapeParam "s2"] $
                       let elem_t = A (AtomTypeVar "t") (ShapeVar "s")
                           acc_t = A (AtomTypeVar "t2") (ShapeVar "s2")
                           op_t = scalar $ arrowType [acc_t, elem_t] acc_t
                           arg_t =
                             A
                               (AtomTypeVar "t")
                               (ShapeDim (DimN 1 <> DimVar "d") <> ShapeVar "s")
                        in scalar $ arrowType [op_t, acc_t, arg_t] acc_t
             ),
             ( "trace",
               scalar $
                 forallType [AtomTypeParam "t", AtomTypeParam "r"] $
                   scalar $
                     piType [ShapeParam "s", ShapeParam "q"] $
                       scalar $
                         arrowType
                           [ A (AtomTypeVar "t") (ShapeVar "s"),
                             A (AtomTypeVar "r") (ShapeVar "q")
                           ]
                           (A (AtomTypeVar "r") (ShapeVar "q"))
             ),
             ( "trace-file",
               scalar $
                 forallType [AtomTypeParam "t", AtomTypeParam "r"] $
                   scalar $
                     piType [DimParam "d", ShapeParam "s", ShapeParam "q"] $
                       scalar $
                         arrowType
                           [ A Int (ShapeDim (DimVar "d")),
                             A (AtomTypeVar "t") (ShapeVar "s"),
                             A (AtomTypeVar "r") (ShapeVar "q")
                           ]
                           (A (AtomTypeVar "r") (ShapeVar "q"))
             ),
             ( "read-file",
               scalar $
                 forallType [AtomTypeParam "t"] $
                   scalar $
                     piType [DimParam "d", ShapeParam "s"] $
                       scalar $
                         arrowType
                           [A Int (ShapeDim (DimVar "d"))]
                           (A (AtomTypeVar "t") (ShapeVar "s"))
             ),
             ( "reify-dim",
               scalar $
                 piType [DimParam "d"] $
                   scalar Int
             ),
             ( "reify-shape",
               scalar $
                 piType [ShapeParam "s"] $
                   scalar $
                     sigmaType [DimParam "r"] $
                       A Int (ShapeDim $ DimVar "r")
             )
           ]
    scalar = mkScalarArrayType
