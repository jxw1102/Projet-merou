package ast.model

import scala.collection.mutable.ArrayBuffer
import ast.SourceCodeNode

abstract class Decl(_name: String) extends SourceCodeNode {
    def name = _name
}

final case class VarDecl     (_name: String, typeName: Type, value: Option[Expr]) extends Decl(_name) 
final case class ParamVarDecl(_name: String, typeName: Type)                      extends Decl(_name) 
final case class FunctionDecl(_name: String, typeName: Type, body: CompoundStmt)  extends Decl(_name) {
    private[this] var _args = List[ParamVarDecl]()
    
    def args: List[ParamVarDecl]            = _args
    def args_=(newArgs: List[ParamVarDecl]) = _args = newArgs
}