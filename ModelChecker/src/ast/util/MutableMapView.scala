package ast.util

import scala.collection.mutable.{Map => MMap}
import scala.collection.mutable.HashMap

/**
 * Implements a view of a mutable map. The map will behave like an immutable map for any usual 
 * use case of an immutable map but will have an additional method to add some key-value 
 * bindings in it, which makes it mutable. This feature is only meant to be used for initial
 * construction, otherwise the map must be treated as immutable.
 * @author David Courtinot
 */
class MutableMapView[K,V] extends Map[K,V] {
	private val _map: MMap[K,V] = HashMap()
	
	// additional method to make the map mutable
	private def  map: Map[K,V]                       = _map.toMap
	def +=(kv: (K,V)): MutableMapView[K,V]           = { _map += kv; this }
	
	// implement the collection.immutable.Map trait
	override def +[V1 >: V](kv: (K, V1)): Map[K, V1] = map + kv
	override def get(key: K): Option[V]              = map.get(key)
	override def iterator: Iterator[(K,V)]           = map.iterator
	override def -(key: K): Map[K,V]                 = map - key
}

object MutableMapView {
    def apply[K,V]             : MutableMapView[K,V] = new MutableMapView[K,V]
    def apply[K,V](kvs: (K,V)*): MutableMapView[K,V] = {
        val res    = MutableMapView[K,V]
        res._map ++= kvs
        res
    }
}