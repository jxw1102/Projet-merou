import ast.model.Decl
import ast.model.Expr

package object cfg {
    implicit def exprToCFGExpr (expr: Expr  ): CFGVal     = CFGExpr(expr)
    implicit def strToCFGString(s   : String): CFGVal     = CFGString(s)
    implicit def strToMetaVar  (s   : String): CFGMetaVar = CFGMetaVar(s)
    implicit def declToCFGDecl (decl: Decl  ): CFGVal     = CFGDecl(decl.id.get,decl.typeOf,decl.name)
}
