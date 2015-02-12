package ctl

import ast.ProgramNodeLabelizer
import cfg.Labelizer
import cfg.Labelizable
import cfg.GraphNode

/**
 * @author Zohour Abouakil
 * @author Fabien Sauce
 */
// Parent class of all the class used in CTL 
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
final case class Exist (name: String, op  : CtlExpr) extends CtlExpr

// Predicate
final case class Predicate (l: ProgramNodeLabelizer) extends CtlExpr 

// Object 
object CtlExpr {
//    def printExpr(expr : CtlExpr) : String = {
//        def formatBinary(s1: String, s2: String, s3: String) = 
//            "(%s %s %s)".format(s1, s2, s3)
//        
//        def formatUnary(s1: String, s2: String) = 
//            "%s(%s)".format(s1, s2)
//            
//        expr match {
//            case And(x, y) => formatBinary(printExpr(x), "&&", printExpr(y))
//            case Or (x, y) => formatBinary(printExpr(x), "||", printExpr(y))
//            case _AU(x, y) => formatBinary(printExpr(x), "AU", printExpr(y))
//            case _EU(x, y) => formatBinary(printExpr(x), "EU", printExpr(y))
//            case AX (x)    => formatUnary("AX", printExpr(x))
//            case EX (x)    => formatUnary("EX", printExpr(x))
//            case AG (x)    => formatUnary("AG", printExpr(x))
//            case EG (x)    => formatUnary("EG", printExpr(x))
//            case AF (x)    => formatUnary("AF", printExpr(x))
//            case EF (x)    => formatUnary("EF", printExpr(x))
//            case Not (x)      => formatUnary("!", printExpr(x))
////            case Predicate(x) => "P" + x.mkString("(", ", ", ")")
//        }
//    }
    
    def evalExpr[U <: Labelizable[V],V <: Labelizer](expr : CtlExpr, modelChecker: ModelChecker[U,V])
                                        : Set[(GraphNode[U,V], Environment)] = {
        
	    expr match {
	        case And   (x, y)    => modelChecker.conj    (evalExpr(x, modelChecker),evalExpr(y, modelChecker))
	        case Or    (x, y)    => modelChecker.disj    (evalExpr(x, modelChecker),evalExpr(y, modelChecker))
	        case _AU   (x, y)    => modelChecker.SAT_AU  (evalExpr(x, modelChecker),evalExpr(y, modelChecker))
	        case _EU   (x, y)    => modelChecker.SAT_EU  (evalExpr(x, modelChecker),evalExpr(y, modelChecker))
	        case AX    (x   )    => modelChecker.preA    (evalExpr(x, modelChecker))
	        case EX    (x   )    => modelChecker.preE    (evalExpr(x, modelChecker))
	        case Not   (x   )    => modelChecker.neg     (evalExpr(x, modelChecker))
            case Exist (x, y)    => modelChecker.exists  (x,evalExpr(y, modelChecker))
	        case Predicate(x: V) => 
                for (n <- modelChecker.nodeParent.states ; env = n.value.visit(x) ; if(env.isDefined)) yield (n,env.get)           
            //case _               => Set[StateEnv]() 
            }
    }
}
