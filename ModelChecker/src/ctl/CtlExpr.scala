package ctl

import ast.Identifier

// Parent class of all the class used un CTL 
abstract class CtlExpr {
    def AU(that: CtlExpr)  = _AU(this,that)
    def EU(that: CtlExpr)  = _EU(this,that)
    def &&(that : CtlExpr) = And(this,that)
    def ||(that : CtlExpr) = Or(this,that)
    def unary_!            = Not(this)       // ! Predicate()
}

// Binary Expression
case class And (left : CtlExpr, right : CtlExpr) extends CtlExpr 
case class Or  (left : CtlExpr, right : CtlExpr) extends CtlExpr 
case class _AU (left : CtlExpr, right : CtlExpr) extends CtlExpr 
case class _EU (left : CtlExpr, right : CtlExpr) extends CtlExpr 

// Unary Expression
case class AX  (right : CtlExpr) extends CtlExpr 
case class EX  (right : CtlExpr) extends CtlExpr 
case class AG  (right : CtlExpr) extends CtlExpr 
case class EG  (right : CtlExpr) extends CtlExpr 
case class AF  (right : CtlExpr) extends CtlExpr 
case class EF  (right : CtlExpr) extends CtlExpr 
case class Not (right : CtlExpr) extends CtlExpr 

// Predicate
case class Predicate (varList : List[Identifier]=List()) extends CtlExpr 

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
}