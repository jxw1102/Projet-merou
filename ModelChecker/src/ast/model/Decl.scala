package ast.model

import scala.collection.mutable.ArrayBuffer
import ast.SourceCodeNode

abstract class Decl(_name: String) extends SourceCodeNode {
    def name = _name
}

final case class VarDecl(_name: String, typeName: String, value: Option[Expr]) extends Decl(_name) {
    override def toString = "%s %s %s".format(typeName,name,if (value.isDefined) " = " + value.get else "")
}

final case class FunctionDecl(_name: String, typeName: String, body: CompoundStmt) extends Decl(_name) {
    private[this] var _args = List[ParamVarDecl]()
    
    def args: List[ParamVarDecl]            = _args
    def args_=(newArgs: List[ParamVarDecl]) = _args = newArgs
    
    override def toString = "%s %s(%s)".format(typeName,name,_args.mkString(", "))
}

final case class ParamVarDecl(_name: String, typeName: String) extends Decl(_name) {
	override def toString = "%s %s".format(typeName,name)
}