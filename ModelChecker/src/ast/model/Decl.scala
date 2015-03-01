package ast.model

import scala.collection.mutable.ArrayBuffer
import ast.SourceCodeNode

/**
 * Classes used to represent main kinds of declarations within the program. They most often correspond to a specific
 * Clang class.
 * @author Sofia Boutahar
 * @author David Courtinot
 * @author Xiaowen Ji
 */
abstract class Decl(_name: String, _typeOf: String) extends SourceCodeNode {
    def name   = _name
    def typeOf = _typeOf
}

final case class VarDecl(_name: String, typeName: String, value: Option[Expr]) extends Decl(_name,typeName) {
    override def toString = "%s %s %s".format(typeName,name,if (value.isDefined) " = " + value.get else "")
}

final case class FunctionDecl(_name: String, typeName: String, args: List[ParamVarDecl], body: CompoundStmt) extends Decl(_name,typeName) {
    override def toString = "%s %s(%s)".format(typeName,name,args.mkString(", "))
}

final case class ParamVarDecl(_name: String, typeName: String) extends Decl(_name,typeName) {
    override def toString = "%s %s".format(typeName,name)
}