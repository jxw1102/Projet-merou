package ast

import scala.collection.mutable.ArrayBuffer

class Stmt extends ProgramNode {
    override def toGraph = ???
}

class CompoundStmt extends Stmt {
    private[this] val body = ArrayBuffer[Stmt]()
    def addStmt(s: Stmt) = body += s
    def statements: Iterable[Stmt] = body ++ ArrayBuffer()
}
class IfStmt        (val condition: Expr, val body: CompoundStmt, val elseStmt: Option[CompoundStmt]) extends Stmt
class Identifier    (val name : String) extends Stmt {
    override def toString = name
}
class FunCall       (val identifier: String, val args: List[Expr]) extends Stmt
class AssignmentStmt(val variable: Identifier, val value: Expr) extends Stmt
class SwitchStmt    (val expr: Expr, val cases: Map[Litteral,SwitchCase], default: Option[CompoundStmt]) extends Stmt
class SwitchCase    (val value: Litteral, val body: CompoundStmt) extends Stmt