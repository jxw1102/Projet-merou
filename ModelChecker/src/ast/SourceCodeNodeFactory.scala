package ast

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map
import ast.model._

/**
 * The SourceCodeNodeFactory, given the data held by an ASTParserResult, converts the ASTNode tree
 * into a higher level tree data-structure, namely SourceCodeNode.
 * @author Sofia Boutahar
 * @author Xiaowen Ji
 * @author David Courtinot
 */
class SourceCodeNodeFactory(root: ASTNode, labels: Map[String,String]) {
    type SCN = SourceCodeNode 
    
    /**
     * labelNodes maps the id of the labels to their corresponding SourceCodeNode conversion
     */
    private val labelNodes = Map[String,SCN]()
    
    // the result of the whole AST's transformation is lazily computed
    lazy val result = new SourceCodeResult(root.children.map(handleASTNode).map(_.asInstanceOf[Decl]),labelNodes)
    
    /**
     * Contains the result of the conversion : the root nodes (declarations out of the main method, and the main itself),
     * and a map mapping the labels' id to their corresponding SourceCodeNode conversion.
     */
    final class SourceCodeResult(val rootNodes: ArrayBuffer[Decl], val labelNodes: Map[String,SCN])
    
    // some utility methods
    private val concreteNodeExpected = (node: ASTNode) => throw new IllegalArgumentException(node + " should be a ConcreteASTNode")
    private val conversionFailed     = (node: ASTNode) => throw new ConversionFailedException("BinaryOperator " + node.mkString)
    private def setAndReturn[T <: SCN](node: T, range: CodeRange, id: String) = { SourceCodeNode(node,range,id); node }
    
    // general facade for handling most kind of nodes
    private def handleASTNode(node: ASTNode): SCN = node match {
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
    
    private def lookFor[T](n: ASTNode, convert: ASTNode => T) = n match {
        case NullASTNode(_) => None
        case x              => Some(convert(n))
    }
    
    // general facade for handling expression nodes
    private def handleExpr(node: ASTNode): Expr = node match {
        case ConcreteASTNode(_,typeOf,_,_,_) => typeOf match {
            case "UnaryOperator"              => unaryOperator            (node)
            case "BinaryOperator"             => binaryOperator           (node)
            case "CallExpr"                   => callExpr                 (node)
            case "DeclRefExpr"                => declRefExpr              (node)
            case "ArraySubscriptExpr"         => arraySubscriptExpr       (node)
            case "InitListExpr"               => initListExpr             (node)
            case "ConditionalOperator"        => ternary                  (node)
            case "CompoundAssignOperator"     => compoundAssignOperator   (node)
            case x if x.contains("Literal")   => literal                  (node)
            case x if x.endsWith("CastExpr")  => handleExpr               (node.children(0))
            case "ParenExpr"                  => handleExpr               (node.children(0))
        }
        case _                                => concreteNodeExpected(node)
    }
    
    private def forStmt(node: ASTNode) = { 
        node match {
            case ConcreteASTNode(_,_,id,codeRange,_) => 
                val init   = lookFor(node.children(0),handleASTNode)
                val cond   = lookFor(node.children(2),handleExpr   )
                val update = lookFor(node.children(3),handleExpr   )
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
            setAndReturn(ParamVarDecl(dataList.get(-2),dataList.get(-1)),codeRange,id)
        case _ => concreteNodeExpected(node)
    }
    
    private def functionDecl(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,id,codeRange,data) => 
            val dataList      = data.dataList
            val args          = node.children.take(node.children.length - 1).map(parmVarDecl).toList 
            val res           = FunctionDecl(dataList.get(-2),dataList.last,args,compoundStmt(node.children.last))
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
        case ConcreteASTNode(_,_,id,codeRange,data) => 
            val dataList = data.dataList
            setAndReturn(Literal(dataList.get(-2),dataList.get(-1)),codeRange,id)
        case _                                      => concreteNodeExpected(node)
    }
    
    private def varDecl(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,id,codeRange,data) => 
            var expr     = if (node.children.isEmpty) None else Some(node.children.map(handleExpr).head)
            val dataList = data.dataList
            val args     = (if (dataList.last == "cinit") List(-3,-2) else List(-2,-1)).map(dataList.get(_))
            SourceCodeNode(VarDecl(args.head,args.last,expr),codeRange,id)
        case _ => concreteNodeExpected(node)
    }
    
    private def declStmt(node: ASTNode): DeclStmt = node match {
        case ConcreteASTNode(_,_,id,codeRange,_) => setAndReturn(DeclStmt(node.children.map(handleASTNode).toList),codeRange,id)
        case _  => concreteNodeExpected(node)
    }
    
    private def unaryOperator(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,id,codeRange,data) => 
            val dataList = data.dataList
            node.children.map(handleASTNode).toList match {
                case (a: Expr) :: Nil => setAndReturn(UnaryOp(dataList.get(-3),a,dataList.get(-1),OpPosition(dataList.get(-2))),codeRange,id)
                case _                => conversionFailed(node)
            }
        case _ => concreteNodeExpected(node)
    }
    
    private def compoundAssignOperator(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,id,codeRange,data) => 
            val dataList = data.dataList
            val x = node.children.map(handleExpr).toList match {
                case (a: Expr) :: (b: Expr) :: Nil => setAndReturn(CompoundAssignOp(dataList.get(-5),a,b,dataList.get(-3)),codeRange,id)
                case _ => conversionFailed(node)
            }
            x
        case _ => concreteNodeExpected(node)
    }
    
    private def binaryOperator(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,id,codeRange,data) => 
            val dataList = data.dataList
            node.children.map(handleASTNode).toList match {
                case (a: Expr) :: (b: Expr) :: Nil => setAndReturn(BinaryOp(dataList.get(-2),a,b,dataList.last),codeRange,id)
                case _                             => conversionFailed(node)
            }
        case _ => concreteNodeExpected(node)
    }
    
    private def arraySubscriptExpr(node: ASTNode) = node match {
         case ConcreteASTNode(_,_,id,codeRange,data) =>
             val tup = (handleExpr(node.children(0)),handleExpr(node.children(1)))
             setAndReturn(ArraySubscriptExpr(data.dataList.get(-2),tup),codeRange,id)
         case _ => concreteNodeExpected(node)
    }
     
    private def initListExpr(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,id,codeRange,data) =>
            setAndReturn(InitListExpr(data.dataList.get(-1),node.children.map(handleExpr).toList),codeRange,id)
        case _ => concreteNodeExpected(node)
    }
    
    private def declRefExpr(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,id,codeRange,data) => 
            val dataList = data.dataList
            val decl     = DeclRefExpr(dataList.last,dataList.get(-2),dataList.get(-3))
            setAndReturn(decl,codeRange,id)
        case _ => concreteNodeExpected(node)
    }
    
    private def callExpr(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,id,codeRange,data) => 
            val children = node.children.map(handleExpr).toList
            setAndReturn(CallExpr(data.dataList.get(-1),children.head.asInstanceOf[DeclRefExpr],children.tail),codeRange,id)
        case _ => concreteNodeExpected(node)
    }
    
    private def returnStmt(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,id,codeRange,_) => 
            val child = node.children.map(handleExpr).toList.last
            setAndReturn(ReturnStmt(child.getType,child),codeRange,id)
        case _ => concreteNodeExpected(node)
    }
    
    private def ternary(node: ASTNode) = node match {
        case ConcreteASTNode(_,_,id,codeRange,data) => 
        	val dataList = data.dataList
            node.children.map(handleExpr).toList match {
                case condition :: yes :: no :: Nil => 
                    setAndReturn(ConditionalOperator(dataList.last,(condition,yes,no)),codeRange,id)
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