package ast

import java.lang.Long.parseLong
import scala.collection.mutable.ArrayBuffer

object StmtFactory {
    
    implicit class DataProcessor(data: String) {
        private def removeQuote(s: String) = {
            s.replaceAll("'", "")
        }
        private val splitReg = "(\\'.+?\\'|\\S+)".r
        lazy    val dataList = splitReg.findAllIn(data).toList.map(removeQuote)
    }
    
    implicit class ListFetcher[T](list: List[T]) {
        def get(idx: Int) = if (idx >= 0) list(idx) else list(list.length+idx)
    }
    
    def handleASTNode(node: ASTNode): ProgramNode = node match {
        case ConcreteASTNode(_,typeOf,_,_,_) => typeOf match {
            case "CompoundStmt"               => compoundStmt  (node)
            case "IfStmt"                     => ifStmt        (node)
            case "ForStmt"                    => forStmt       (node)
            case "ExprStmt"                   => handleExpr    (node)
            case "DeclStmt"                   => declStmt      (node)
            case "VarDecl"                    => varDecl       (node)
            case "ReturnStmt"                 => returnStmt    (node)
            case _                            => handleExpr    (node)
        }
        case OtherASTNode(depth,data)         => ???
    }
    
    def handleForInitializer(node: ASTNode): ForInitializer = node match {
        case ConcreteASTNode(_,typeOf,_,_,_) => typeOf match {
            case "DeclStmt" => declStmt(node)
            case _          => handleExpr(node)
        }
    }
    
    def handleExpr(node: ASTNode): Expr = node match {
        case ConcreteASTNode(_,typeOf,_,_,_) => typeOf match {
            case "UnaryOperator"              => unaryOperator (node).asInstanceOf[Expr]
            case "BinaryOperator"             => binaryOperator(node).asInstanceOf[Expr]
            case "ImplicitCastExpr"           => ignore        (node).asInstanceOf[Expr]
            case "CallExpr"                   => callExpr      (node).asInstanceOf[Expr]
            case "CStyleCastExpr"             => ignore        (node).asInstanceOf[Expr]
            case "DeclRefExpr"                => declRefExpr   (node).asInstanceOf[Expr]
            case "ConditionalOperator"        => conditionalOperator(node).asInstanceOf[Expr]
            case "CompoundAssignOperator"     => compoundAssignOperator(node).asInstanceOf[Expr]
            case tp if tp.endsWith("Literal") => literal       (node).asInstanceOf[Expr]
        }
        case OtherASTNode(depth,data)         => ???
    }
    
    def forStmt(node: ASTNode) = {
        def lookFor[T](n: ASTNode, convert: ASTNode => T) = n match {
            case NullASTNode(_) => None
            case x              => Some(convert(n))
        }
        node match {
            case ConcreteASTNode(_,_,id,codeRange,_) => {
                val init   = lookFor(node.children(0),handleForInitializer)
                val cond   = lookFor(node.children(2),handleExpr            )
                val update = lookFor(node.children(3),handleExpr            )
                val body   = compoundStmt(node.children(4))
                ForStmt(init,cond,update,body)
            }
        }
    }
    
    def ifStmt(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,id,codeRange,_) => {
            val cond     = handleExpr(node.children(1))
            val body     = compoundStmt(node.children(2))
            val elseStmt = node.children(3) match {
                case ConcreteASTNode(_,_,_,_,_) => Some(compoundStmt(node.children(3)))
                case _                          => None
            }
            ProgramNode(IfStmt(cond,body,elseStmt),codeRange,id)
        }
        case _ => throw new IllegalArgumentException("node should be a ConcreteASTNode")
    }
    
    def compoundStmt(node: ASTNode): CompoundStmt = CompoundStmt(node.children.map(handleASTNode).toList)
    
    def literal(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,id,codeRange,data) => {
            ProgramNode(Litteral(data.dataList.last),codeRange,id)
        }
        case _ => throw new IllegalArgumentException("node should be a ConcreteASTNode")
    }
    
    def varDecl(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,id,codeRange,data) => {
            var expr: Option[Expr] = None
            try {
              expr = Some(node.children.map(handleExpr).last)
            } catch {
              case t: Throwable => t.printStackTrace()
            }
            val dataList = data.dataList
            ProgramNode(VarDecl(dataList.get(-2),Type(dataList.last),expr),codeRange,id)
        }
        case _ => throw new IllegalArgumentException("node should be a ConcreteASTNode")
    }
    
    def declStmt(node: ASTNode): DeclStmt = node match {
        case ConcreteASTNode(_,_,id,codeRange,_) => 
            val res = DeclStmt(node.children.map(handleASTNode).toList)
            ProgramNode(res,codeRange,id)
            res
        case _                                   => throw new IllegalArgumentException("node should be a ConcreteASTNode")
    }
    
    def binaryOperator(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,id,codeRange,data) => {
            node.children.map(handleExpr).toList match {
                case (a: Expr) :: (b: Expr) :: Nil => ProgramNode(BinaryOp(a,b,data.dataList.last),codeRange,id)
                case _ => throw new ConversionFailedException("BinaryOperator " + node.mkString)
            }
        }
        case _ => throw new IllegalArgumentException("node should be a ConcreteASTNode")
    }
    
    def unaryOperator(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,id,codeRange,data) => {
            node.children.map(handleASTNode).toList match {
                case (a: Expr) :: Nil => ProgramNode(UnaryOp(a,data.dataList.last),codeRange,id)
                case _ => throw new ConversionFailedException("BinaryOperator " + node.mkString)
            }
        }
        case _ => throw new IllegalArgumentException("node should be a ConcreteASTNode")
    }
    
    def compoundAssignOperator(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,id,codeRange,data) => {
            node.children.map(handleExpr).toList match {
                case (a: Expr) :: (b: Expr) :: Nil => ProgramNode(CompoundAssignOp(a,b,data.dataList.last),codeRange,id)
                case _ => throw new ConversionFailedException("BinaryOperator " + node.mkString)
            }
        }
        case _ => throw new IllegalArgumentException("node should be a ConcreteASTNode")
    }
    
    def ignore(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,_,_,_) => node.children.map(handleASTNode).last
        case _ => throw new IllegalArgumentException("node should be a ConcreteASTNode")
    }
    
    def declRefExpr(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,id,codeRange,data) => {
            val dataList = data.dataList
            ProgramNode(DeclRefExpr(dataList.last,dataList.get(-2),parseLong(dataList.get(-3).substring(2),16),dataList.get(-4)),codeRange,id)
        }
        case _ => throw new IllegalArgumentException("node should be a ConcreteASTNode")
    }
    
    def callExpr(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,id,codeRange,data) => ProgramNode(CallExpr(data.dataList.last,node.children.map(handleExpr).toList),codeRange,id)
        case _ => throw new IllegalArgumentException("node should be a ConcreteASTNode")
    }
    
    def returnStmt(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,id,codeRange,_) => ProgramNode(ReturnStmt(node.children.map(handleExpr).toList.last),codeRange,id)
        case _ => throw new IllegalArgumentException("node should be a ConcreteASTNode")
    }
    
    def conditionalOperator(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,id,codeRange,data) => ProgramNode(ConditionalOperator(node.children.map(handleExpr).toList,data.dataList.last),codeRange,id)
        case _ => throw new IllegalArgumentException("node should be a ConcreteASTNode")
    }
}