package ast

import scala.collection.mutable.ArrayBuffer

abstract class Decl(_name: String) extends ProgramNode {
    def name = _name
}

final case class VarDecl     (_name: String, typeName: Type, value: Option[Expr]) extends Decl(_name) 
final case class ParamVarDecl(_name: String, typeName: Type)                     extends Decl(_name) 
final case class FunctionDecl(_name: String, typeName: Type, body: CompoundStmt) extends Decl(_name) {
    private[this] val _args: ArrayBuffer[ParamVarDecl] = ArrayBuffer()
    
    def args: Iterable[ParamVarDecl] = _args ++ List()
    def args_+= = None
}