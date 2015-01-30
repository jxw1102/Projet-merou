package ast

import scala.collection.mutable.ArrayBuffer
import ast.util.MutableListView

class Stmt extends ProgramNode 
final case class CompoundStmt() extends Stmt {
    val  body: MutableListView[Stmt] = MutableListView()
}

final case class AssignmentStmt(variable: String, value: Expr)                                              extends Stmt
final case class IfStmt        (condition: Expr, body: CompoundStmt, elseStmt: Option[CompoundStmt])        extends Stmt
final case class SwitchStmt    (expr: Expr, cases: Map[Litteral,SwitchCase], default: Option[CompoundStmt]) extends Stmt
final case class SwitchCase    (value: Litteral, body: CompoundStmt)                                        extends Stmt
final case class DeclStmt      (decl: Decl)                                                                 extends Stmt
final case class FunCall       (identifier: String)                                                         extends Stmt {
    val  args: MutableListView[Expr] = MutableListView()
}

final case class Type(name: String)