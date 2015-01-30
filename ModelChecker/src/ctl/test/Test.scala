package ctl.test

import ctl._


/**
 * @author Zohour Abouakil
 */
// Object traite les objets
object Main extends App {

    val p1 = Predicate(List("x"))
    val p2 = Predicate(List("y"))

    val expr = new AX(AX(p1) && EG(p2))

    println(CtlExpr.printExpr(expr))

}
