package ast.model

import ast.SourceCodeNode

/**
 * Classes used to represent different kinds of loops within the program
 * @author Sofia Boutahar
 * @author David Courtinot
 * @author Xiaowen Ji
 */
trait ForInitializer
sealed trait LoopStmt
final case class EnhancedForStmt(iterable: String, loopVar: Decl, body: Stmt)                                                extends Stmt with LoopStmt
final case class ForStmt        (init: Option[SourceCodeNode], cond: Option[Expr], update: Option[Expr], body: Stmt)         extends Stmt with LoopStmt
final case class WhileStmt      (condition: Expr, body: Stmt)                                                                extends Stmt with LoopStmt
final case class DoWhileStmt    (condition: Expr, body: Stmt)                                                                extends Stmt with LoopStmt
