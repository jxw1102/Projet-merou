package ast

import java.lang.Long.parseLong
import scala.collection.mutable.ArrayBuffer

/**
 * This class provides all necessary methods to convert an ASTNode into a SourceCodeNode
 * @author Sofia Boutahar
 * @author Xiaowen Ji
 * @author David Courtinot
 */
object SourceCodeNodeFactory {
    val concreteNodeExpected = () => throw new IllegalArgumentException("node should be a ConcreteASTNode")
    
    /**
     * Implicit declatation that enables to split the 'data' field of the ConcreteASTNode(s)
     */
    private implicit class DataProcessor(data: String) {
        val dataList = DataProcessor.splitReg.findAllIn(data).map(_.replaceAll("'", "")).toList
    }
    
    private implicit object DataProcessor {
        private val splitReg = "(\\'.+?\\'|\\S+)".r
    }
    
    // not very efficient to perform random access on a List
    private implicit class ListFetcher[T](list: List[T]) {
        def get(idx: Int) = if (idx >= 0) list(idx) else list(list.length + idx)
    }
    
    /**
     * General facade for handling most kind of nodes
     */
    def handleASTNode(node: ASTNode): SourceCodeNode = node match {
        case ConcreteASTNode(_,typeOf,_,_,_) => typeOf match {
            case "CompoundStmt"               => compoundStmt  (node)
            case "IfStmt"                     => ifStmt        (node)
            case "ForStmt"                    => forStmt       (node)
            case "ExprStmt"                   => exprStmt      (node)
            case "DeclStmt"                   => declStmt      (node)
            case "VarDecl"                    => varDecl       (node)
            case "BinaryOperator"             => binaryOperator(node)
            case "CallExpr"                   => callExpr      (node)
            case "DeclRefExpr"                => declRefExpr   (node)
            case "ReturnStmt"                 => returnStmt    (node)
            case "ConditionalOperator"        => ternary       (node)
            case x if x.endsWith("Literal")   => literal       (node)
            case x if x.endsWith("CastExpr")  => handleASTNode (node.children(0))
        }
        case _                                => concreteNodeExpected()
    }
    
    private def handleForInitializer(node: ASTNode): ForInitializer = node match {
        case ConcreteASTNode(_,typeOf,_,_,_) => typeOf match {
            case "DeclStmt" => declStmt(node)
            case _          => exprStmt(node)
        }
        case _ => concreteNodeExpected()
    }
    
    private def forStmt(node: ASTNode) = { 
        def lookFor[T](n: ASTNode, convert: ASTNode => T) = n match {
            case NullASTNode(_) => None
            case x              => Some(convert(n))
        }
        node match {
            case ConcreteASTNode(_,_,id,codeRange,_) => {
                val init   = lookFor(node.children(0),handleForInitializer)
                val cond   = lookFor(node.children(2),exprStmt            )
                val update = lookFor(node.children(3),exprStmt            )
                val body   = compoundStmt(node.children(4))
                ForStmt(init,cond,update,body)
            }
            case _ => concreteNodeExpected()
        }
    }
    
    private def ifStmt(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,id,codeRange,_) => {
            val cond     = exprStmt(node.children(1))
            val body     = compoundStmt(node.children(2))
            val elseStmt = node.children(3) match {
                case ConcreteASTNode(_,_,_,_,_) => Some(compoundStmt(node.children(3)))
                case _                          => None
            }
            SourceCodeNode(IfStmt(cond,body,elseStmt),codeRange,id)
        }
        case _ => concreteNodeExpected()
    }
    
    private def compoundStmt(node: ASTNode): CompoundStmt = CompoundStmt(node.children.map(handleASTNode).toList)
    private def exprStmt(node: ASTNode) = ???
    
    private def literal(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,id,codeRange,data) => SourceCodeNode(Litteral(data.dataList.head),codeRange,id)
        case _                                      => concreteNodeExpected()
    }
    
    private def varDecl(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,id,codeRange,data) => {
            var expr: Option[Expr] = None
            // What is that ? Seems to be an ugly hack (with no offense)
            try {
              expr = Some(node.children.map(handleASTNode).last.asInstanceOf[Expr])
            } catch {
              case t: Throwable => t.printStackTrace()
            }
            val dataList = data.dataList
            SourceCodeNode(VarDecl(dataList.get(-2),Type(dataList.last),expr),codeRange,id)
        }
        case _ => concreteNodeExpected()
    }
    
    private def declStmt(node: ASTNode): DeclStmt = node match {
        case ConcreteASTNode(_,_,id,codeRange,_) => 
            val res = DeclStmt(node.children.map(handleASTNode).toList)
            SourceCodeNode(res,codeRange,id)
            res
        case _  => concreteNodeExpected()
    }
    
    private def binaryOperator(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,id,codeRange,data) => {
            node.children.map(handleASTNode).toList match {
                case (a: Expr) :: (b: Expr) :: Nil => SourceCodeNode(BinaryOp(a,b,data.dataList.last),codeRange,id)
                case _ => throw new ConversionFailedException("BinaryOperator " + node.mkString)
            }
        }
        case _ => concreteNodeExpected()
    }
    
    private def declRefExpr(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,id,codeRange,data) => 
            val dataList = data.dataList
            val decl     = DeclRefExpr(dataList.last,dataList.get(-2),parseLong(dataList.get(-3).substring(2),16),dataList.get(-4))
            SourceCodeNode(decl,codeRange,id)
        case _ => concreteNodeExpected()
    }
    
    private def callExpr(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,id,codeRange,data) => 
            SourceCodeNode(CallExpr(data.dataList.last,node.children.map(handleASTNode).toList.asInstanceOf[List[Expr]]),codeRange,id)
        case _ => concreteNodeExpected()
    }
    
    private def returnStmt(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,id,codeRange,_) => 
            SourceCodeNode(ReturnStmt(node.children.map(handleASTNode).toList.last.asInstanceOf[Expr]),codeRange,id)
        case _ => concreteNodeExpected()
    }
    
    private def ternary(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,id,codeRange,data) => 
            node.children.map(handleASTNode).toList match {
                case (condition: Expr) :: (yes: Expr) :: (no: Expr) :: Nil => 
                    SourceCodeNode(ConditionalOperator((condition,yes,no),data.dataList.last),codeRange,id)
            }
            
        case _ => concreteNodeExpected()
    }
}