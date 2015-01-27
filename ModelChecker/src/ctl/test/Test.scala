package ctl.test

import ast.Identifier
import ctl._


/**
 * @author simple
 */

// Object traite les objets
object Main  extends App {
    
  val p1 = Predicate(List(new Identifier("x")))
  val p2 = Predicate(List(new Identifier("y")))
  
  val expr = new AX(AX(p1) && EG(p2))
  
  println(CtlExpr.printExpr(expr))
  
}
