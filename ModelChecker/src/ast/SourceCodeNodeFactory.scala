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
    private val concreteNodeExpected = (node: ASTNode) => throw new IllegalArgumentException(node + " should be a ConcreteASTNode")
    private def setAndReturn[T <: SourceCodeNode](node: T, range: CodeRange, id: Long) = {
        SourceCodeNode(node,range,id)
        node
    }
    
    /**
     * Implicit declaration that enables to split the 'data' field of the ConcreteASTNode(s)
     */
    private implicit class DataProcessor(data: String) {
        val dataList = DataProcessor.splitReg.findAllIn(data).map(_.replaceAll("'", "")).toSeq
    }
    
    private implicit object DataProcessor {
        private val splitReg = "(\\'.+?\\'|\\S+)".r
    }
    
    private implicit class SeqFetcher[T](seq: Seq[T]) {
        def get(idx: Int) = if (idx >= 0) seq(idx) else seq(seq.length + idx)
    }
    
    /**
     * General facade for handling most kind of nodes
     */
    def handleASTNode(node: ASTNode): SourceCodeNode = node match {
        case ConcreteASTNode(_,typeOf,_,_,_) => typeOf match {
            case "CompoundStmt"               => compoundStmt  (node)
            case "IfStmt"                     => ifStmt        (node)
            case "ForStmt"                    => forStmt       (node)
            case "DeclStmt"                   => declStmt      (node)
            case "VarDecl"                    => varDecl       (node)
            case "ReturnStmt"                 => returnStmt    (node)
            case "BreakStmt"                  => breakStmt     (node)
            case "ContinueStmt"               => continueStmt  (node)
            case "SwitchStmt"                 => switchStmt    (node)
            case "LabelStmt"                  => labelStmt     (node)
            case "GotoStmt"                   => gotoStmt      (node)
            case "NullStmt"                   => nullStmt      (node)
            case _                            => handleExpr    (node)
        }
        case _                                => concreteNodeExpected(node)
    }
    
    private def handleForInitializer(node: ASTNode): ForInitializer = node match {
        case ConcreteASTNode(_,typeOf,_,_,_) => typeOf match {
            case "DeclStmt" => declStmt(node)
            case _          => handleExpr(node)
        }
        case _ => concreteNodeExpected(node)
    }
    
    def handleExpr(node: ASTNode): Expr = node match {
        case ConcreteASTNode(_,typeOf,_,_,_) => typeOf match {
            case "UnaryOperator"              => unaryOperator            (node)
            case "BinaryOperator"             => binaryOperator           (node)
            case "CallExpr"                   => callExpr                 (node)
            case "DeclRefExpr"                => declRefExpr              (node)
            case "ConditionalOperator"        => ternary                  (node)
            case "CompoundAssignOperator"     => compoundAssignOperator   (node)
            case x if x.endsWith("Literal")   => literal                  (node)
            case x if x.endsWith("CastExpr")  => handleExpr               (node.children(0))
        }
        case _                                => concreteNodeExpected(node)
    }
    
    private def forStmt(node: ASTNode) = { 
        def lookFor[T](n: ASTNode, convert: ASTNode => T) = n match {
            case NullASTNode(_) => None
            case x              => Some(convert(n))
        }
        
        node match {
            case ConcreteASTNode(_,_,id,codeRange,_) => {
                val init   = lookFor(node.children(0),handleForInitializer)
                val cond   = lookFor(node.children(2),handleExpr          )
                val update = lookFor(node.children(3),handleExpr          )
                val body   = compoundStmt(node.children(4))
                ForStmt(init,cond,update,body)
            }
            case _ => concreteNodeExpected(node)
        }
    }
    
    private def ifStmt(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,id,codeRange,_) => {
            val cond     = handleExpr  (node.children(1))
            val body     = compoundStmt(node.children(2))
            val elseStmt = node.children(3) match {
                case ConcreteASTNode(_,_,_,_,_) => Some(handleASTNode(node.children(3)).asInstanceOf[Stmt])
                case _                          => None
            }
            SourceCodeNode(IfStmt(cond,body,elseStmt),codeRange,id)
        }
        case _ => concreteNodeExpected(node)
    }
    
    private def compoundStmt(node: ASTNode): CompoundStmt = CompoundStmt(node.children.map(handleASTNode).toList)
    
    private def literal(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,id,codeRange,data) => setAndReturn(Literal(data.dataList.head),codeRange,id)
        case _                                      => concreteNodeExpected(node)
    }
    
    private def varDecl(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,id,codeRange,data) => {
            var expr = if (node.children.isEmpty) None else Some(node.children.map(handleExpr).head)
            val dataList = data.dataList
            SourceCodeNode(VarDecl(dataList.get(-2),Type(dataList.last),expr),codeRange,id)
        }
        case _ => concreteNodeExpected(node)
    }
    
    private def declStmt(node: ASTNode): DeclStmt = node match {
        case ConcreteASTNode(_,_,id,codeRange,_) => 
            setAndReturn(DeclStmt(node.children.map(handleASTNode).toList),codeRange,id)
        case _  => concreteNodeExpected(node)
    }
    
    def unaryOperator(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,id,codeRange,data) => {
            node.children.map(handleASTNode).toList match {
                case (a: Expr) :: Nil => setAndReturn(UnaryOp(a,data.dataList.last),codeRange,id)
                case _ => throw new ConversionFailedException("UnaryOperator " + node.mkString)
            }
        }
        case _ => throw new IllegalArgumentException("node should be a ConcreteASTNode")
    }
    
    def compoundAssignOperator(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,id,codeRange,data) => {
            node.children.map(handleExpr).toList match {
                case (a: Expr) :: (b: Expr) :: Nil => setAndReturn(CompoundAssignOp(a,b,data.dataList.last),codeRange,id)
                case _ => throw new ConversionFailedException("CompoundAssignOperator " + node.mkString)
            }
        }
        case _ => throw new IllegalArgumentException("node should be a ConcreteASTNode")
    }
    
    private def binaryOperator(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,id,codeRange,data) => {
            node.children.map(handleASTNode).toList match {
                case (a: Expr) :: (b: Expr) :: Nil => setAndReturn(BinaryOp(a,b,data.dataList.last),codeRange,id)
                case _                             => throw new ConversionFailedException("BinaryOperator " + node.mkString)
            }
        }
        case _ => concreteNodeExpected(node)
    }
    
    private def declRefExpr(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,id,codeRange,data) => 
            val dataList = data.dataList
            val decl     = DeclRefExpr(dataList.last,dataList.get(-2),parseLong(dataList.get(-3).substring(2),16),dataList.get(-4))
            setAndReturn(decl,codeRange,id)
        case _ => concreteNodeExpected(node)
    }
    
    private def callExpr(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,id,codeRange,data) => 
            setAndReturn(CallExpr(data.dataList.last,node.children.map(handleExpr).toList),codeRange,id)
        case _ => concreteNodeExpected(node)
    }
    
    private def returnStmt(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,id,codeRange,_) => 
        	setAndReturn(ReturnStmt(node.children.map(handleExpr).toList.last),codeRange,id)
        case _ => concreteNodeExpected(node)
    }
    
    private def ternary(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,id,codeRange,data) => 
            node.children.map(handleExpr).toList match {
                case condition :: yes :: no :: Nil => 
                    setAndReturn(ConditionalOperator((condition,yes,no),data.dataList.last),codeRange,id)
            }
            
        case _ => concreteNodeExpected(node)
    }
    
    private def breakStmt(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,id,codeRange,_) => 
            setAndReturn(ReturnStmt(node.children.map(handleExpr).toList.last),codeRange,id)
        case _ => concreteNodeExpected(node)
    }
    
    private def continueStmt(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,id,codeRange,_) => 
            setAndReturn(ReturnStmt(node.children.map(handleExpr).toList.last),codeRange,id)
        case _ => concreteNodeExpected(node)
    }
    
    private def labelStmt(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,id,codeRange,data) => 
            setAndReturn(LabelStmt(data.dataList.last,node.children.map(handleExpr).toList.last),codeRange,id)
        case _ => concreteNodeExpected(node)
    }
    
    private def gotoStmt(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,id,codeRange,data) => 
            setAndReturn(GotoStmt(data.dataList.get(-2)),codeRange,id)
        case _ => concreteNodeExpected(node)
    }
    
    private def nullStmt(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,id,codeRange,_) => 
            setAndReturn(NullStmt(),codeRange,id)
        case _ => concreteNodeExpected(node)
    }
    
    private def switchStmt(node: ASTNode) = {
        node match {
            case ConcreteASTNode(_, _, id, codeRange, _) => {
                // expr: Expr, cases: Map[Literal,SwitchCase], default: Option[CompoundStmt]
                val expr = handleExpr(node.children(1))
                val cases = CompoundStmt(node.children(2).children.map(handleASTNode).toList)
                val default = node.children.last match {
                    case ConcreteASTNode(_, _, _, _, _) => Some(handleASTNode(node.children.last).asInstanceOf[Stmt])
                    case _                              => None
                }
                SwitchStmt(expr, cases, default)
            }
            case _ => concreteNodeExpected(node)
        }
    }
    
}