import ast.model.Expr
import ast.model.DeclRefExpr

package object cfg {
    implicit def exprToCFGExpr(e: Expr  )     : CFGVal = CFGExpr(e)
    implicit def strToCFGDecl (d: DeclRefExpr): CFGVal = CFGDecl(d)
}
