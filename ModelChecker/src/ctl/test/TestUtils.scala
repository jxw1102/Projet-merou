package ctl.test

import ctl.MetaVariable
import ctl.Value

/**
 * Utility class for automated testing
 * @author David Courtinot
 */
trait TestUtils {
	private var i = 0
    def printMsg(failed: Boolean)     = { 
	    val msg = "\tTest %d %s".format(i,if (failed) "failed" else "passed") 
	    if (failed) Console.err.println(msg)
	    else        println(msg)
	    i += 1
	}
    def assertEquals[T](a0: T, a1: T) = printMsg(a0 != a1)
    def assertTrue     (b: Boolean)   = printMsg(!b)
    
    implicit def strToId(s: String): Identifier = Identifier(s)
    
    def newTestSession = i = 0
}

case class Identifier(name: String) extends Value with MetaVariable {
    override def hashCode       = name.hashCode
    override def equals(a: Any) = a match {
        case Identifier(value) => value == name
        case _                 => false 
    }
    override def toString       = name
}