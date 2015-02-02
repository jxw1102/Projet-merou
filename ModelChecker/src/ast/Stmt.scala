package ast

import ast.util.MutableListView

class Stmt extends ProgramNode 
final case class AssignmentStmt(variable: String, value: Expr)                                              extends Stmt
final case class CompoundStmt  (elts: List[ProgramNode])                                                    extends Stmt 
final case class IfStmt        (condition: Expr, body: CompoundStmt, elseStmt: Option[CompoundStmt])        extends Stmt
final case class SwitchStmt    (expr: Expr, cases: Map[Litteral,SwitchCase], default: Option[CompoundStmt]) extends Stmt
final case class SwitchCase    (value: Litteral, body: CompoundStmt)                                        extends Stmt
final case class DeclStmt      (decls: List[ProgramNode])                                                   extends Stmt with ForInitializer
final case class ReturnStmt    (expr: Expr)    extends Stmt
final case class Type          (name: String)
