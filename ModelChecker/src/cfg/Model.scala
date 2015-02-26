package cfg

import scala.reflect.runtime.universe
import ast.Expression
import ast.For
import ast.If
import ast.ProgramNode
import ast.Statement
import ast.While
import ast.model._
import ctl.BindingsEnv
import ctl.Bottom
import ctl.ConvertEnv
import ctl.Environment
import ctl.Labelizer
import ctl.MetaVariable
import ctl.TypeOf
import ctl.Value
import ast.Switch

/* 
 * /////////////////////// Generic type definitions : Value and MetaVariable ///////////////////////
 */
case class CFGMetaVar(name: String) extends MetaVariable {
    override def hashCode       = name.hashCode
    override def toString       = name

    override def equals(a: Any) = a match {
        case CFGMetaVar(value) => value == name
        case _                 => false 
    }
}

sealed abstract class CFGVal extends TypeOf[CFGVal] with Value
final case class CFGExpr(expr: Expr) extends CFGVal {
    override def toString        = expr.toString
    override def cast(n: CFGVal) = n match { case CFGExpr(_) => true; case _ => false }    
}
/**
 * CFGDecl represents a C++ declaration. The equality between two CFGDecl is based on their ID in
 * the AST.
 */
final case class CFGDecl(id: String, typeOf: String, name: String) extends CFGVal {
    override def cast(n: CFGVal) = n match { case CFGDecl(_,_,_) => true; case _ => false }
    override val hashCode        = id.hashCode
    override def equals(a: Any)  = a match { case CFGDecl(id,_,_) => id == this.id; case _ => false }
}
/**
 * CFGDef, just like CFGDecl represents a C++ declaration. However, while CFGDecl corresponds to an
 * actual declaration, CFGDef only represents the semantic of a declaration and not the declaration
 * itself. Indeed, two CFGDef are considered equal if they declare a variable of the same name and 
 * the same type.
 */
final case class CFGDef(typeOf: String, name: String) extends CFGVal {
    override def cast(n: CFGVal) = n match { case CFGDef(_,_) => true; case _ => false }
}
final case class CFGString(s: String) extends CFGVal {
    override def cast(n: CFGVal) = n match { case CFGString(_) => true; case _ => false }
}

object ConvertNodes {
    private def getAllExpr(expr: Expr): Set[CFGVal] = expr.getSubExprs.map(CFGExpr(_)).toSet + CFGExpr(expr)
    
    // TODO : mettre à jour avec les nouveaux types de valeurs
    def convert: (ProgramNode => Set[CFGVal]) = (p: ProgramNode) => p match {
        case If        (expr,_,_)       => getAllExpr(expr)
        case While     (expr,_,_)       => getAllExpr(expr)
        case Expression(expr,_,_)       => getAllExpr(expr)
        case Switch    (expr,_,_)       => getAllExpr(expr)
        case For       (Some(expr),_,_) => getAllExpr(expr)
        case Statement (decl: VarDecl,_,_) => 
//            val res: Set[CFGVal] = Set(CFGExpr(DeclRefExpr(decl.typeName, decl.name, decl.id.get, "Var")))
            if (decl.value.isDefined) Set(CFGExpr(decl.value.get)) else Set()
        case _                          => Set()
    }
    
    def getAllExprs(node: ProgramNode) = convert(node)
    	.filter(_.isInstanceOf[CFGExpr])
    	// just to avoid the warning in the next map
    	.map(_.asInstanceOf[CFGExpr])
    	.map { case CFGExpr(expr) => expr }.toList
}