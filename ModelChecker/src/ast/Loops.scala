package ast

sealed trait ForInitializer
class EnhancedForStmt(val iterable: Identifier, loopVar: Decl, body: CompoundStmt) extends Stmt
class ForStmt        (val init: Option[ForInitializer], val condition: Option[Expr], val update: Option[Expr], body: CompoundStmt) extends Stmt
class WhileStmt      (val condition: Expr, val body: CompoundStmt) extends Stmt
class DoWhileStmt    (condition: Expr, body: CompoundStmt) extends WhileStmt(condition,body) {
    override def toGraph = ???
}
