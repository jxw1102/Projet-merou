package ast

trait ForInitializer
class ForStmt    (val init: Option[ForInitializer], val condition: Option[Expr], val update: Option[Expr], body: CompoundStmt) extends Stmt
class WhileStmt  (val condition: Expr, val body: CompoundStmt, val elseStmt: Option[CompoundStmt]) extends Stmt
class DoWhileStmt(val condition: Expr, val body: CompoundStmt, val elseStmt: Option[CompoundStmt]) extends Stmt
