import ast.model.Expr
import ast.model.Decl

package object cfg {
    implicit def exprToCFGExpr(e: Expr  ): CFGVal     = CFGExpr   (e)
    implicit def strToCFGDecl (s: String): CFGVal     = CFGDecl   (s)
    implicit def strToMetaVar (s: String): CFGMetaVar = CFGMetaVar(s)
}
