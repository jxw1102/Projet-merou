import ast.model.Expr
import ast.model.Decl


package object cfg {
    implicit def exprToCFGExpr (e: Expr        ): CFGVal = CFGExpr(e)
    implicit def declToCFGDecl (d: Decl        ): CFGVal = CFGDecl(d)
    implicit def exprToCFGBlock(l: List[CFGVal]): CFGVal = CFGBlock(l)

}