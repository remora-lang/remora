module {
  func.func @main() -> tensor<1xi64> {
    %cst = arith.constant dense<5> : tensor<1xi64>
    %cst_0 = arith.constant dense<3> : tensor<1xi64>
    %0 = call @"diff-square-sum_x[1]_y[1]"(%cst, %cst_0) : (tensor<1xi64>, tensor<1xi64>) -> tensor<1xi64>
    return %0 : tensor<1xi64>
  }
  func.func @"diff-square-sum_x[1]_y[1]"(%arg0: tensor<1xi64>, %arg1: tensor<1xi64>) -> tensor<1xi64> {
    %0 = tensor.empty() : tensor<1xi64>
    %1 = linalg.mul ins(%arg0, %arg0 : tensor<1xi64>, tensor<1xi64>) outs(%0 : tensor<1xi64>) -> tensor<1xi64>
    %2 = tensor.empty() : tensor<1xi64>
    %3 = linalg.mul ins(%arg1, %arg1 : tensor<1xi64>, tensor<1xi64>) outs(%2 : tensor<1xi64>) -> tensor<1xi64>
    %4 = tensor.empty() : tensor<1xi64>
    %5 = linalg.sub ins(%1, %3 : tensor<1xi64>, tensor<1xi64>) outs(%4 : tensor<1xi64>) -> tensor<1xi64>
    %6 = tensor.empty() : tensor<1xi64>
    %7 = linalg.mul ins(%arg0, %arg0 : tensor<1xi64>, tensor<1xi64>) outs(%6 : tensor<1xi64>) -> tensor<1xi64>
    %8 = tensor.empty() : tensor<1xi64>
    %9 = linalg.mul ins(%arg1, %arg1 : tensor<1xi64>, tensor<1xi64>) outs(%8 : tensor<1xi64>) -> tensor<1xi64>
    %10 = tensor.empty() : tensor<1xi64>
    %11 = linalg.sub ins(%7, %9 : tensor<1xi64>, tensor<1xi64>) outs(%10 : tensor<1xi64>) -> tensor<1xi64>
    %12 = tensor.empty() : tensor<1xi64>
    %13 = linalg.add ins(%5, %11 : tensor<1xi64>, tensor<1xi64>) outs(%12 : tensor<1xi64>) -> tensor<1xi64>
    return %13 : tensor<1xi64>
  }
}