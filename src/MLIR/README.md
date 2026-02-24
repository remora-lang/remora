# Walk Futhark IR and emit MLIR, or maybe XDSL

# XDSL

https://github.com/xdslproject/xdsl


# MLIR notes

MLIR ref: https://mlir.llvm.org/docs/LangRef/

MLIR cheat sheet (from Stephen Diehl, https://www.stephendiehl.com/posts/mlir_introduction/
   also, pages 15, 16 of https://llvm.org/devmtg/2020-09/slides/MLIR_Tutorial.pdf

  % prefix: SSA values (e.g. %result)
  @ prefix: Functions (e.g. @fun)
  ^ prefix: Basic blocks (e.g. ^bb0)
  # prefix: Attribute aliases (e.g. #map_1d_identity)
  x delimiter: Used in shapes and between shape and type (e.g. 10xf32)
  : and -> are used to indicate the type of an operation or value (e.g. %result: i32)
  ! prefix: Type aliases (e.g. !avx_m128 = vector<4 x f32>)
  () are used to represent arguments (e.g. (%arg0, %arg1))
  {} are used to represent regions
  // is used for comments
  <> are used to indicate type parameters (e.g. tensor<10xf32>)

module def: module { operations }
function def: func.func @my_function(%arg0: i32, %arg1: i32) -> i32 { operations }
operation: %0:2 = "my_dialect.my_operation"(%arg0#3, %arg1){ some.attribute = true  } : 
                   (!mydialect<"some_type">, i32) -> (!mydialect<"other_type">, i32)
  ":2" is number of values returned"
  "#3" is index in producer's results

basic block: ^bb1: // Label for the then block
             %then_result = arith.muli %result, 2 : i32
             return %then_result : i32

region: { ^bb1(%result : i32):
           %then_result = arith.muli %result, 2 : i32
           return %then_result : i32 }

type synonym: !avx_m128 = vector<4 x f32>
