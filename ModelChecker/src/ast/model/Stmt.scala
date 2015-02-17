package ast.model

import ast.SourceCodeNode

/**
 * Classes used to represent main kinds of statements within the program
 * @author Sofia Boutahar
 * @author David Courtinot
 * @author Xiaowen Ji
 */
class Stmt extends SourceCodeNode 
sealed trait     JumpStmt                                                                         extends Stmt
final case class SwitchStmt    (expr: Expr, body: CompoundStmt)                                   extends Stmt
final case class CaseStmt      (condition: Expr, body: Stmt)                                      extends Stmt
final case class DefaultStmt   (body: Stmt)                                                       extends Stmt
final case class IfStmt        (condition: Expr, body: Stmt, elseStmt: Option[Stmt])              extends Stmt
final case class CompoundStmt  (elts: List[SourceCodeNode])                                         extends Stmt 
final case class LabelStmt     (label: String, stmt: Stmt)                                        extends Stmt {
    override def toString = "LabelStmt"
}
final case class NullStmt      ()                                                                 extends Stmt
final case class ContinueStmt  ()                                                                 extends JumpStmt
final case class BreakStmt     ()                                                                 extends JumpStmt
final case class GotoStmt      (label: String)                                                    extends JumpStmt
final case class ReturnStmt    (expr: Expr)                                                       extends JumpStmt
final case class DeclStmt      (decls: List[SourceCodeNode])                                      extends ForInitializer {
    override def toString = decls.mkString(", ")
}
