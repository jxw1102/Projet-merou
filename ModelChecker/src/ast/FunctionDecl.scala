package ast

class FunctionDecl {
    var name: String
    var typename: Type
    var args: List[ParamVarDecl] = List()
    var body: CompoundStmt = CompoundStmt()
}