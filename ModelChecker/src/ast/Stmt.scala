package ast

class Stmt {

}

abstract class CompoundStmt(var body: List[Stmt])
class IfStmt(val condition: Expr, val body: CompoundStmt, val elseStmt: Option[CompoundStmt]) extends Stmt
