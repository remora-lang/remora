module Intrinsics (intrinsics, maxIntrinsicTag) where

import Control.Monad
import Data.Bifunctor
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Prettyprinter
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
      <$> ( forM allFuncs $
              \(v, t) -> do
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
               mkScalarArrayType $
                 Forall
                   [AtomTypeParam "t"]
                   ( A
                       ( Pi
                           [DimParam "d", ShapeParam "s"]
                           ( A
                               ( [ A
                                     (AtomTypeVar "t")
                                     ( Concat
                                         [ ShapeDim (Add [DimN 1, DimVar "d"]),
                                           ShapeVar "s"
                                         ]
                                     )
                                 ]
                                   :-> A (AtomTypeVar "t") (ShapeVar "s")
                               )
                               mempty
                           )
                       )
                       mempty
                   )
             ),
             ( "tail",
               mkScalarArrayType $
                 Forall
                   [AtomTypeParam "t"]
                   ( A
                       ( Pi
                           [DimParam "d", ShapeParam "s"]
                           ( A
                               ( [ A
                                     (AtomTypeVar "t")
                                     ( Concat
                                         [ ShapeDim (Add [DimN 1, DimVar "d"]),
                                           ShapeVar "s"
                                         ]
                                     )
                                 ]
                                   :-> A (AtomTypeVar "t") (ShapeVar "s")
                               )
                               mempty
                           )
                       )
                       mempty
                   )
             ),
             ( "length",
               mkScalarArrayType $
                 Forall
                   [AtomTypeParam "t"]
                   ( A
                       ( Pi
                           [DimParam "d", ShapeParam "s"]
                           ( A
                               ( [A (AtomTypeVar "t") (Concat [ShapeDim (DimVar "d"), ShapeVar "s"])]
                                   :-> A Int mempty
                               )
                               mempty
                           )
                       )
                       mempty
                   )
             ),
             ( "append",
               mkScalarArrayType $
                 Forall
                   [AtomTypeParam "t"]
                   ( A
                       ( Pi
                           [DimParam "m", DimParam "n", ShapeParam "s"]
                           ( A
                               ( [ A (AtomTypeVar "t") (Concat [ShapeDim $ DimVar "m", ShapeVar "s"]),
                                   A (AtomTypeVar "t") (Concat [ShapeDim $ DimVar "n", ShapeVar "s"])
                                 ]
                                   :-> A
                                     (AtomTypeVar "t")
                                     ( Concat
                                         [ ShapeDim $ Add [DimVar "m", DimVar "n"],
                                           ShapeVar "s"
                                         ]
                                     )
                               )
                               mempty
                           )
                       )
                       mempty
                   )
             ),
             ( "reverse",
               mkScalarArrayType $
                 Forall
                   [AtomTypeParam "t"]
                   ( A
                       ( Pi
                           [DimParam "d", ShapeParam "s"]
                           ( let arr_t =
                                   A
                                     (AtomTypeVar "t")
                                     ( Concat
                                         [ShapeDim $ DimVar "d", ShapeVar "s"]
                                     )
                              in A ([arr_t] :-> arr_t) mempty
                           )
                       )
                       mempty
                   )
             ),
             ( "reduce",
               mkScalarArrayType $
                 Forall
                   [AtomTypeParam "t"]
                   ( A
                       ( Pi
                           [DimParam "d", ShapeParam "s"]
                           ( let elem_type = A (AtomTypeVar "t") (ShapeVar "s")
                                 op_type = A ([elem_type, elem_type] :-> elem_type) mempty
                                 arg_type =
                                   A
                                     (AtomTypeVar "t")
                                     (ShapeDim (DimN 1 <> DimVar "d") <> ShapeVar "s")
                                 res_type = A (AtomTypeVar "t") (ShapeVar "s")
                              in A ([op_type, arg_type] :-> res_type) mempty
                           )
                       )
                       mempty
                   )
             ),
             ( "sum",
               mkScalarArrayType $
                 Pi
                   [ShapeParam "s"]
                   ( A
                       ( [A Int (ShapeVar "s")]
                           :-> A Int mempty
                       )
                       mempty
                   )
             ),
             ( "flatten",
               mkScalarArrayType $
                 Forall
                   [AtomTypeParam "t"]
                   ( A
                       ( Pi
                           [DimParam "m", DimParam "n", ShapeParam "s"]
                           ( let arg_t =
                                   A
                                     (AtomTypeVar "t")
                                     ( Concat
                                         [ShapeDim $ DimVar "m", ShapeDim $ DimVar "n", ShapeVar "s"]
                                     )
                                 ret_t =
                                   A
                                     (AtomTypeVar "t")
                                     ( Concat
                                         [ShapeDim $ (Mul [DimVar "m", DimVar "n"]), ShapeVar "s"]
                                     )
                              in A ([arg_t] :-> ret_t) mempty
                           )
                       )
                       mempty
                   )
             ),
             ( "iota",
               mkScalarArrayType $
                 Pi
                   [DimParam "d"]
                   ( let arg_t = A Int (ShapeDim $ DimVar "d")
                         ret_t =
                           mkScalarArrayType $
                             Sigma [ShapeParam "s"] $
                               A Int (ShapeVar "s")
                      in mkScalarArrayType $ [arg_t] :-> ret_t
                   )
             ),
             ( "iota/static",
               mkScalarArrayType $
                 Pi [ShapeParam "s"] $
                   A Int (ShapeVar "s")
             ),
             ( "transpose2d",
               mkScalarArrayType $
                 Forall
                   [AtomTypeParam "t"]
                   ( mkScalarArrayType $
                       ( Pi
                           [DimParam "m", DimParam "n"]
                           ( let arg_t = A (AtomTypeVar "t") (Concat $ map (ShapeDim . DimVar) ["m", "n"])
                                 ret_t = A (AtomTypeVar "t") (Concat $ map (ShapeDim . DimVar) ["n", "m"])
                              in mkScalarArrayType $ [arg_t] :-> ret_t
                           )
                       )
                   )
             ),
             ( "undefined",
               mkScalarArrayType $
                 Forall
                   [AtomTypeParam "t"]
                   (mkScalarArrayType (Pi [ShapeParam "s"] $ A (AtomTypeVar "t") (ShapeVar "s")))
             ),
             ( "index2d",
               mkScalarArrayType $
                 Forall [AtomTypeParam "t"] $
                   mkScalarArrayType
                     ( Pi
                         [DimParam "m", DimParam "n"]
                         ( let arr_t = A (AtomTypeVar "t") $ Concat $ map (ShapeDim . DimVar) ["m", "n"]
                               idx_t = A Int $ ShapeDim $ DimN 2
                               ret_t = A (AtomTypeVar "t") $ Concat []
                            in mkScalarArrayType $ [arr_t, idx_t] :-> ret_t
                         )
                     )
             ),
             ( "i->f",
               mkScalarArrayType ([A Int $ Concat []] :-> (A Float $ Concat []))
             )
           ]
