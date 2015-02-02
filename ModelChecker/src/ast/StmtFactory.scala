package ast

import java.lang.Long.parseLong

import scala.collection.mutable.ArrayBuffer

class StmtFactory {
    def handleASTNode(node: ASTNode): ProgramNode = node match {
        case ConcreteASTNode(_,typeOf,_,_,_) => typeOf match {
            case "CompoundStmt"               => compoundStmt  (node)
            case "IfStmt"                     => ifStmt        (node)
            case "ForStmt"                    => forStmt       (node)
            case "ExprStmt"                   => exprStmt      (node)
            case "VarDecl"                    => varDecl       (node)
            case "BinaryOperator"             => binaryOperator(node)
            case "ImplicitCastExpr"           => ignore        (node)
            case "CStyleCastExpr"             => ignore        (node)
            case "DeclRefExpr"                => declRefExpr   (node)
            case "ReturnStmt"                 => returnStmt    (node)
            case "ConditionalOperator"        => conditionalOperator(node)
            case tp if tp.endsWith("Literal") => literal       (node)
        }
        case OtherASTNode(depth,data)        => ???
    }
    
    def forStmt(node: ASTNode) = ???
    def ifStmt(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,id,codeRange,_) => {
            val cond     = exprStmt(node.children(1))
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
    def exprStmt(node: ASTNode) = ???
    
    def literal(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,id,codeRange,data) => {
            ProgramNode(Litteral(data.split(" ").last),codeRange,id)
        }
        case _ => throw new IllegalArgumentException("node should be a ConcreteASTNode")
    }
    
    def varDecl(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,id,codeRange,data) => {
            var expr: Option[Expr] = None
            try {
              expr = Some(node.children.map(handleASTNode).last.asInstanceOf[Expr])
            } catch {
              case t: Throwable => t.printStackTrace()
            }
            val dataList = data.split(" ")
            ProgramNode(VarDecl(dataList(dataList.length - 2),Type(dataList.last),expr),codeRange,id)
        }
        case _ => throw new IllegalArgumentException("node should be a ConcreteASTNode")
    }
    
    def declStmt(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,id,codeRange,_) => {
            ProgramNode(DeclStmt(node.children.map(handleASTNode).toList),codeRange,id)
        }
        case _ => throw new IllegalArgumentException("node should be a ConcreteASTNode")
    }
    
    def binaryOperator(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,id,codeRange,_) => {
            node.children.map(handleASTNode).toList match {
                case (a: Expr) :: (b: Expr) :: Nil => ProgramNode(BinaryOp(a,b),codeRange,id)
                case _ => throw new ConversionFailedException("BinaryOperator " + node.mkString)
            }
        }
        case _ => throw new IllegalArgumentException("node should be a ConcreteASTNode")
    }
    
    def ignore(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,_,_,_) => {
            node.children.map(handleASTNode).last
        }
        case _ => throw new IllegalArgumentException("node should be a ConcreteASTNode")
    }
    
    def declRefExpr(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,id,codeRange,data) => {
            val dataList = data.split(" ")
            ProgramNode(DeclRefExpr(dataList(dataList.length-2),dataList.last,parseLong(dataList(dataList.length-3).substring(2)),dataList(dataList.length-4)),codeRange,id)
        }
        case _ => throw new IllegalArgumentException("node should be a ConcreteASTNode")
    }
    
    def callExpr(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,id,codeRange,data) => {
            val dataList = data.split(" ")
            ProgramNode(CallExpr(dataList.last,node.children.map(handleASTNode).toList.asInstanceOf[List[Expr]]),codeRange,id)
        }
        case _ => throw new IllegalArgumentException("node should be a ConcreteASTNode")
    }
    
    def returnStmt(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,id,codeRange,_) => {
            ProgramNode(ReturnStmt(node.children.map(handleASTNode).toList.last.asInstanceOf[Expr]),codeRange,id)
        }
        case _ => throw new IllegalArgumentException("node should be a ConcreteASTNode")
    }
    
    def conditionalOperator(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,id,codeRange,data) => {
            ProgramNode(ConditionalOperator(node.children.map(handleASTNode).toList.asInstanceOf[List[Expr]],data.split(" ").last),codeRange,id)
        }
        case _ => throw new IllegalArgumentException("node should be a ConcreteASTNode")
    }
    
}