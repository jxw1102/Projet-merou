package ast

sealed trait ForInitializer
class EnhancedForStmt(val iterable: Identifier, loopVar: Decl, body: CompoundStmt) extends Stmt
class ForStmt        (val init: Option[ForInitializer], val condition: Option[Expr], val update: Option[Expr], body: CompoundStmt) extends Stmt
class WhileStmt      (val condition: Expr, val body: CompoundStmt) extends Stmt
class DoWhileStmt    (condition: Expr, body: CompoundStmt) extends WhileStmt(condition,body) {
<<<<<<< HEAD
    override def toGraph = ???
=======
    override def toGraph = throw new UnsupportedOperationException
>>>>>>> 9740ac969fed04ee9070792d7c16d43797f7e80c
}
