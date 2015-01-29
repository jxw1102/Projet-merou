package ast

abstract class Decl extends ProgramNode
class FunctionDecl(val name: Identifier, val typeName: Type, val body: CompoundStmt) {
    var args: List[ParamVarDecl] = List()
}
class ParamVarDecl(val typeName: Type, val name: String)
class VarDecl(val typeName: Type, val name: Identifier, val value: Expr)