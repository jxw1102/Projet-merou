package ast

/**
 * Classes used to represent main kinds of statements within the program
 * @author Sofia Boutahar
 * @author David Courtinot
 * @author Xiaowen Ji
 */
class Stmt extends SourceCodeNode 
final case class SwitchStmt    (expr: Expr, cases: Map[Literal,SwitchCase], default: Option[CompoundStmt])  extends Stmt
final case class IfStmt        (condition: Expr, body: CompoundStmt, elseStmt: Option[Stmt])                extends Stmt
final case class SwitchCase    (value: Literal, body: CompoundStmt)                                         extends Stmt
final case class AssignmentStmt(variable: String, value: Expr)                                              extends Stmt
final case class DeclStmt      (decls: List[SourceCodeNode])                                                extends Stmt with ForInitializer
final case class CompoundStmt  (elts: List[SourceCodeNode])                                                 extends Stmt 
final case class ReturnStmt    (expr: Expr)                                                                 extends Stmt
final case class Type          (name: String)
