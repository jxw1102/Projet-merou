package ctl

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
    def unary_!            = Not(this)
}

// Binary operators
final case class And (left: CtlExpr, right: CtlExpr) extends CtlExpr 
final case class Or  (left: CtlExpr, right: CtlExpr) extends CtlExpr 
final case class _AU (left: CtlExpr, right: CtlExpr) extends CtlExpr 
final case class _EU (left: CtlExpr, right: CtlExpr) extends CtlExpr 

// Unary operators
final case class AX                  (op  : CtlExpr) extends CtlExpr 
final case class EX                  (op  : CtlExpr) extends CtlExpr 
final case class AG                  (op  : CtlExpr) extends CtlExpr 
final case class EG                  (op  : CtlExpr) extends CtlExpr 
final case class AF                  (op  : CtlExpr) extends CtlExpr 
final case class EF                  (op  : CtlExpr) extends CtlExpr 
final case class Not                 (op  : CtlExpr) extends CtlExpr 
final case class Exists(name: String, op  : CtlExpr) extends CtlExpr

// Predicate
final case class Predicate (varList : List[String]=List()) extends CtlExpr 

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
    
//        def evalExpr(expr : CtlExpr) : ModelChecker.CheckerResult = {
//	        expr match {
//	            case And(x, y) => ModelChecker.conj(evalExpr(x), evalExpr(y))
//	            case Or (x, y) => ModelChecker.disj(evalExpr(x), evalExpr(y))
//	            case _AU(x, y) => 
//	            case _EU(x, y) => 
//	            case AX (x)    => 
//	            case EX (x)    => 
//	            case AG (x)    => 
//	            case EG (x)    => 
//	            case AF (x)    => 
//	            case EF (x)    => 
//	            case Not (x)   => 
//	            case Predicate(x) => 
//	        }
//    }
}
