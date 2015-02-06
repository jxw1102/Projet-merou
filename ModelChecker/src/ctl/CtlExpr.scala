package ctl

import cfg.Labelizer

0

/**
 * @author Zohour Abouakil
 * @author Fabien Sauce
 */
// Parent class of all the class used un CTL 
sealed abstract class CtlExpr {
    
    def AU(that: CtlExpr)  = _AU(this,that)
    def EU(that: CtlExpr)  = _EU(this,that)
    def &&(that : CtlExpr) = And(this,that)
    def ||(that : CtlExpr) = Or(this,that)
    def unary_!            = Not(this)       // ! Predicate()
    
    //     Re-use some operator
//    def AF(that: CtlExpr)  = _AU(true,that)
//    def EF(that: CtlExpr)  = _EU(true,that)
//    def AG(that: CtlExpr)  = Not(EF(Not(that)))
//    def EG(that: CtlExpr)  = Not(AF(Not(that)))
    
}

// Binary Expression
final case class And (left : CtlExpr, right : CtlExpr) extends CtlExpr

final case class Or  (left : CtlExpr, right : CtlExpr) extends CtlExpr 
final case class _AU (left : CtlExpr, right : CtlExpr) extends CtlExpr 
final case class _EU (left : CtlExpr, right : CtlExpr) extends CtlExpr 

// Unary Expression
final case class AX    (right : CtlExpr)              extends CtlExpr 
final case class EX    (right : CtlExpr)              extends CtlExpr 
final case class AG    (right : CtlExpr)              extends CtlExpr    /* replaced */
final case class EG    (right : CtlExpr)              extends CtlExpr    /* replaced */
final case class AF    (right : CtlExpr)              extends CtlExpr    /* replaced */
final case class EF    (right : CtlExpr)              extends CtlExpr    /* replaced */
final case class Not   (right : CtlExpr)              extends CtlExpr 
final case class Exists(varName: String, op: CtlExpr) extends CtlExpr

// Predicate
final case class Predicate[L <: Labelizer] (labelizer: L) extends CtlExpr 

// Object 
object CtlExpr {
    def printExpr(expr : CtlExpr) : String = {
        def formatBinary(s1: String, s2: String, s3: String) = 
            "(%s %s %s)".format(s1, s2, s3)
        
        def formatUnary(s1: String, s2: String) = 
            "%s(%s)".format(s1, s2)
            
        expr match {
            case And(x, y) => formatBinary(printExpr(x), "&&", printExpr(y))
            case Or (x, y) => formatBinary(printExpr(x), "||", printExpr(y))
            case _AU(x, y) => formatBinary(printExpr(x), "AU", printExpr(y))
            case _EU(x, y) => formatBinary(printExpr(x), "EU", printExpr(y))
            case AX (x)    => formatUnary("AX", printExpr(x))
            case EX (x)    => formatUnary("EX", printExpr(x))
            case AG (x)    => formatUnary("AG", printExpr(x))
            case EG (x)    => formatUnary("EG", printExpr(x))
            case AF (x)    => formatUnary("AF", printExpr(x))
            case EF (x)    => formatUnary("EF", printExpr(x))
            case Not (x)   => formatUnary("!", printExpr(x))
            case Predicate(x) => "P" + x.mkString("(", ", ", ")")
        }
    }
    
    
    def evalExpr(expr : CtlExpr) : ModelChecker.CheckerResult = {
            
        expr match {
            case And(x, y) => ModelChecker.conj(evalExpr(x), evalExpr(y))
            case Or (x, y) => ModelChecker.disj(evalExpr(x), evalExpr(y))
            case _AU(x, y) => 
            case _EU(x, y) => 
            case AX (x)    => 
            case EX (x)    => 
            case AG (x)    => 
            case EG (x)    => 
            case AF (x)    => 
            case EF (x)    => 
            case Not (x)   => 
            case Predicate(x) => 
        }
    }
}
