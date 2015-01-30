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
    
    def codeRange                     = _codeRange
    def codeRange_=(range: CodeRange) = _codeRange = Some(range)
}
object ProgramNode {
    def apply(node: ProgramNode, codeRange: CodeRange) = node.codeRange = codeRange
//    def apply(node: ASTNode) = node match {
//        case ConcreteASTNode(depth,ofType,id,pos,data) => 
//            val instance: ProgramNode = ofType match {
//                case "CompoundStmt" => new CompoundStmt
////                case "CallExpr"     => new FunCall()
//            }
//            instance.codeRange_=(pos)
//            instance
////        case NullASTNode =>
////        case OtherASTNode =>
//    }
}