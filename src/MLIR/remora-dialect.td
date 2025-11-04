// tablegen input file defining Remora mlir dialect
def Remora_Dialect : Dialect {
    let name = "Remora";
    let summary = "MLIR dialect for Remora programming language";
    let description = "Assumes all calls have been monomorphised.";
    let cppNameSpace = "remora";



}
