import ast.model.Expr
import ast.model.DeclRefExpr
import ast.model.Decl

package object cfg {
    implicit def exprToCFGExpr (expr: Expr  ): CFGVal = CFGExpr(expr)
    implicit def strToCFGString(s   : String): CFGVal = CFGString(s)
    implicit def declToCFGDecl (decl: Decl  ): CFGVal = CFGDecl(decl.id.get,decl.typeOf,decl.name)
}
