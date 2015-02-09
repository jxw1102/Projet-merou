package ast

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map
import ast.model._

/**
 * This class provides all necessary methods to convert an ASTNode into a SourceCodeNode
 * @author Sofia Boutahar
 * @author Xiaowen Ji
 * @author David Courtinot
 */
class SourceCodeNodeFactory(root: ASTNode, labels: Map[String,String]) {
    private val labelNodes = Map[String,SourceCodeNode]()
    
    // the result of the whole AST's transformation is lazily computed
    lazy val result = new SourceCodeNodeResult(root.children.map(handleASTNode), labelNodes)
    
    // some utility methods
    private val concreteNodeExpected = (node: ASTNode) => throw new IllegalArgumentException(node + " should be a ConcreteASTNode")
    private val conversionFailed     = (node: ASTNode) => throw new ConversionFailedException("BinaryOperator " + node.mkString)
    private def setAndReturn[T <: SourceCodeNode](node: T, range: CodeRange, id: String) = {
        SourceCodeNode(node,range,id)
        node
    }
    
    final class SourceCodeNodeResult(val rootNodes: ArrayBuffer[SourceCodeNode], val labelNodes: Map[String,SourceCodeNode])
    
    /**
     * General facade for handling most kind of nodes
     */
    private def handleASTNode(node: ASTNode): SourceCodeNode = node match {
           case ConcreteASTNode(_,typeOf,_,_,_) => typeOf match {
               case "CompoundStmt"   => compoundStmt(node)
               case "IfStmt"         => ifStmt      (node)
               case "ForStmt"        => forStmt     (node)
               case "WhileStmt"      => whileStmt   (node)
               case "DoStmt"         => doWhileStmt (node)
               case "DeclStmt"       => declStmt    (node)
               case "VarDecl"        => varDecl     (node)
               case "FunctionDecl"   => functionDecl(node)
               case "ReturnStmt"     => returnStmt  (node)
               case "BreakStmt"      => breakStmt   (node)
               case "ContinueStmt"   => continueStmt(node)
               case "SwitchStmt"     => switchStmt  (node)
               case "LabelStmt"      => labelStmt   (node)
               case "GotoStmt"       => gotoStmt    (node)
               case "CaseStmt"       => caseStmt    (node)
               case "DefaultStmt"    => defaultStmt (node)
               case "NullStmt"       => nullStmt    (node)
               case _                => handleExpr  (node)
           }
           case _                    => concreteNodeExpected(node)
    }
    
    def lookFor[T](n: ASTNode, convert: ASTNode => T) = n match {
            case NullASTNode(_) => None
            case x              => Some(convert(n))
    }
    
    def handleExpr(node: ASTNode): Expr = node match {
        case ConcreteASTNode(_,typeOf,_,_,_) => typeOf match {
            case "UnaryOperator"              => unaryOperator            (node)
            case "BinaryOperator"             => binaryOperator           (node)
            case "CallExpr"                   => callExpr                 (node)
            case "DeclRefExpr"                => declRefExpr              (node)
            case "ArraySubscriptExpr"         => arraySubscriptExpr       (node)
            case "InitListExpr"               => initListExpr             (node)
            case "ConditionalOperator"        => ternary                  (node)
            case "CompoundAssignOperator"     => compoundAssignOperator   (node)
            case x if x.endsWith("Literal")   => literal                  (node)
            case x if x.endsWith("CastExpr")  => handleExpr               (node.children(0))
            case "ParenExpr"                  => handleExpr               (node.children(0))
        }
        case _                                => concreteNodeExpected(node)
    }
    
    private def forStmt(node: ASTNode) = { 
        node match {
            case ConcreteASTNode(_,_,id,codeRange,_) => 
                val init   = lookFor(node.children(0),handleASTNode                      )
                val cond   = lookFor(node.children(2),handleExpr                         )
                val update = lookFor(node.children(3),handleExpr                         )
                val body   = handleASTNode(node.children(4)).asInstanceOf[Stmt]
                SourceCodeNode(ForStmt(init,cond,update,body),codeRange,id)
            case _ => concreteNodeExpected(node)
        }
    }
    
    private def whileStmt(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,id,codeRange,_) => 
            val condition = handleExpr(node.children(1))
            val body      = handleASTNode(node.children(2)).asInstanceOf[Stmt]
            SourceCodeNode(WhileStmt(condition,body),codeRange,id)
        case _ => concreteNodeExpected(node)
    }
    
    private def doWhileStmt(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,id,codeRange,_) => 
            val condition = handleExpr(node.children(1))
            val body = handleASTNode(node.children(0)).asInstanceOf[Stmt]
            SourceCodeNode(DoWhileStmt(condition,body),codeRange,id)
        case _ => concreteNodeExpected(node)
    }
    
    private def parmVarDecl(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,id,codeRange,data) => 
           val dataList = data.dataList
            setAndReturn(ParamVarDecl(dataList.get(-2),Type(dataList.get(-1))),codeRange,id)
        case _ => concreteNodeExpected(node)
    }
    
    private def functionDecl(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,id,codeRange,data) => 
            val dataList = data.dataList
            val res  = FunctionDecl(dataList.get(-3),Type(dataList.get(-2)),compoundStmt(node.children.last))
            res.args = node.children.slice(0,node.children.length - 1).map(parmVarDecl).toList      
            setAndReturn(res,codeRange,id)
        case _ => concreteNodeExpected(node)
    }
    
    private def ifStmt(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,id,codeRange,_) => 
            val cond     = handleExpr   (node.children(1))
            val body     = handleASTNode(node.children(2)).asInstanceOf[Stmt]
            val elseStmt = node.children(3) match {
                case ConcreteASTNode(_,_,_,_,_) => Some(handleASTNode(node.children(3)).asInstanceOf[Stmt])
                case _                          => None
            }
            SourceCodeNode(IfStmt(cond,body,elseStmt),codeRange,id)
        case _ => concreteNodeExpected(node)
    }
    
    private def compoundStmt(node: ASTNode): CompoundStmt = node match {
        case ConcreteASTNode(_,_,id,codeRange,_) => setAndReturn(CompoundStmt(node.children.map(handleASTNode).toList),codeRange,id)
        case _                                   => concreteNodeExpected(node)
    }
    
    private def literal(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,id,codeRange,data) => setAndReturn(Literal(data.dataList.head),codeRange,id)
        case _                                      => concreteNodeExpected(node)
    }
    
    private def varDecl(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,id,codeRange,data) => 
            var expr = if (node.children.isEmpty) None else Some(node.children.map(handleExpr).head)
            val dataList = data.dataList
            SourceCodeNode(VarDecl(dataList.get(-2),Type(dataList.last),expr),codeRange,id)
        case _ => concreteNodeExpected(node)
    }
    
    private def declStmt(node: ASTNode): DeclStmt = node match {
        case ConcreteASTNode(_,_,id,codeRange,_) => setAndReturn(DeclStmt(node.children.map(handleASTNode).toList),codeRange,id)
        case _  => concreteNodeExpected(node)
    }
    
    def unaryOperator(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,id,codeRange,data) => 
            node.children.map(handleASTNode).toList match {
                case (a: Expr) :: Nil => setAndReturn(UnaryOp(a,data.dataList.last),codeRange,id)
                case _                => conversionFailed(node)
            }
        case _ => concreteNodeExpected(node)
    }
    
    def compoundAssignOperator(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,id,codeRange,data) => 
            node.children.map(handleExpr).toList match {
                case (a: Expr) :: (b: Expr) :: Nil => setAndReturn(CompoundAssignOp(a,b,data.dataList.last),codeRange,id)
                case _ => conversionFailed(node)
            }
        case _ => concreteNodeExpected(node)
    }
    
    private def binaryOperator(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,id,codeRange,data) => 
            node.children.map(handleASTNode).toList match {
                case (a: Expr) :: (b: Expr) :: Nil => setAndReturn(BinaryOp(a,b,data.dataList.last),codeRange,id)
                case _                             => conversionFailed(node)
            }
        case _ => concreteNodeExpected(node)
    }
    
    private def arraySubscriptExpr(node: ASTNode) = node match {
    	 case ConcreteASTNode(_,_,id,codeRange,data) =>
    	 	val tup = (handleExpr(node.children(0)),handleExpr(node.children(1)))
    	 	setAndReturn(ArraySubscriptExpr(tup),codeRange,id)
    	 case _ => concreteNodeExpected(node)
    }
     
    private def initListExpr(node: ASTNode) = node match {
    	case ConcreteASTNode(_,_,id,codeRange,data) =>
    		setAndReturn(InitListExpr(node.children.map(handleExpr).toList),codeRange,id)
    	case _ => concreteNodeExpected(node)
    }
    
    private def declRefExpr(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,id,codeRange,data) => 
            val dataList = data.dataList
            val decl     = DeclRefExpr(dataList.last,dataList.get(-2),dataList.get(-3),dataList.get(-4))
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
                case _ => conversionFailed(node)
            }
        case _ => concreteNodeExpected(node)
    }
    
    private def breakStmt(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,id,codeRange,_) => setAndReturn(BreakStmt(),codeRange,id)
        case _                                   => concreteNodeExpected(node)
    }
    
    private def continueStmt(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,id,codeRange,_) => setAndReturn(ContinueStmt(),codeRange,id)
        case _                                   => concreteNodeExpected(node)
    }
    
    private def gotoStmt(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,id,codeRange,data) => setAndReturn(GotoStmt(data.dataList.get(-2)),codeRange,id)
        case _                                      => concreteNodeExpected(node)
    }
    
    private def nullStmt(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,id,codeRange,_) => setAndReturn(NullStmt(),codeRange,id)
        case _                                   => concreteNodeExpected(node)
    }
    
    private def switchStmt(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,id,codeRange,_) => 
            val expr = handleExpr(node.children(1))
            val body = compoundStmt(node.children(2))
            setAndReturn(SwitchStmt(expr,body),codeRange,id)
        case _ => concreteNodeExpected(node)
    }
    
    private def labelStmt(node: ASTNode) = node match {
    	case ConcreteASTNode(_,_,id,codeRange,data) =>
    		val res = setAndReturn(
    		        LabelStmt(data.dataList.last,node.children.map(handleASTNode).head.asInstanceOf[Stmt]),codeRange,id)
    		labelNodes += labels(id) -> res
    		res
    	case _ => concreteNodeExpected(node)
    }
    
    private def caseStmt(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,id,codeRange,_) => 
            val condition = handleExpr(node.children(0))
            val body      = handleASTNode(node.children(2)).asInstanceOf[Stmt]
            setAndReturn(CaseStmt(condition,body),codeRange,id)
        case _ => concreteNodeExpected(node)
    }
    
    private def defaultStmt(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,id,codeRange,_) => 
            val body = handleASTNode(node.children(0)).asInstanceOf[Stmt]
            setAndReturn(DefaultStmt(body),codeRange,id)
        case _ => concreteNodeExpected(node)
    }
}