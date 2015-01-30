package ast

import scala.collection.mutable.HashMap
import scala.collection.mutable.{Map => MMap}

import cfg.GraphNode
import util.MutableMapView

class Program {
    type MMV = MutableMapView[String,Decl]
    
    /**
     * View of the declarations of the program. The returned map will behave like
     * an immutable map for any usual usage of an immutable map but will have an additional
     * method to add some key-value bindings in it, which makes it mutable. This feature is
     * only meant to be used for initial construction, otherwise the map must be treated as
     * immutable.
     */
    val declarations: MMV = MutableMapView()
    
}

abstract class ProgramNode 
object ProgramNode {
    def apply(node: ASTNode) = node match {
    	case ConcreteASTNode(depth,ofType,id,pos,data) => 
            ofType match {
                case "CompoundStmt" => new CompoundStmt
                case "DeclStmt"     => new DeclStmt
            }
    }
}