package ast

import scala.collection.mutable.HashMap
import scala.collection.mutable.{Map => MMap}

import cfg.GraphNode
import util.MutableMapView

class Program {
    type MMV = MutableMapView[String,Decl]
    val declarations: MMV = MutableMapView()
    
}

abstract class ProgramNode {
    private[this] var _codeRange: Option[CodeRange] = None
    private[this] var _id       : Option[Long]      = None
    
    def codeRange                     = _codeRange
    def codeRange_=(range: CodeRange) = _codeRange = Some(range)
    
    def id             = _id
    def id_=(id: Long) = _id = Some(id)
}
object ProgramNode {
    def apply(node: ProgramNode, codeRange: CodeRange, id: Long) = { node.codeRange = codeRange; node.id = id; node }
}