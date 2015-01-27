package ast

class FunctionDecl(var name: String, var typename: Type, var body: CompoundStmt) {
    var args: List[ParamVarDecl] = List()
}