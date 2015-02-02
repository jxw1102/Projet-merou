package ast.util

import ast.Stmt
import scala.collection.mutable.ArrayBuffer

/**
 * Implements a view of an ArrayBuffer. The collection is still limited but with limited features 
 * (appending at the end only). The mutability of this class is only meant to be used for initial
 * construction, otherwise the collection must be treated as immutable (e.g converted to a List).
 * @author David Courtinot
 */
class MutableListView[T] {
    private val buffer = ArrayBuffer[T]()
    def +=(t: T)       = buffer += t
	def toList         = buffer.toList
}  
  
object MutableListView {
	def apply[T]       : MutableListView[T] = new MutableListView
	def apply[T](t: T*): MutableListView[T] = {
	    val res = MutableListView[T]()
	    MutableListView().buffer ++= t
	    res
	}
}