package cfg

import java.io.PrintWriter
import ast.ProgramNodeFactory
import ast.ASTParser
import ast.SourceCodeNodeFactory
import scala.sys.process._
import java.io.File
import ast.ProgramNode
import ctl.ModelChecker
import ctl.ModelChecker
import ctl.AX
import ctl.Predicate
import ctl.EX
import ast.model.Literal
import ast.model.DeclRefExpr
import ctl.EU
import ctl.AU


/**
 * @author Zohour Abouakil 
 * @author David Courtinot
 */
object Main extends App {
    def buildGraph(filePath: String, fileName: String, dot: String="dot") = {
        val cmd    = "clang -Xclang -ast-dump -fsyntax-only " + filePath
        val basePath = filePath.substring(0,filePath.indexOf(fileName))
        val clangPath = basePath + fileName + ".txt"
        println(clangPath)
        
        var writer = new PrintWriter(clangPath)
        writer.write(cmd.!!)
        writer.close
        
        // parse the AST
        val parseResult = (new ASTParser).parseFile(clangPath)
        val astRes      = new SourceCodeNodeFactory(parseResult.root,parseResult.labels).result 

        // generate the CFG and write it in a file
//        new ProgramNodeFactory(astRes.rootNodes,astRes.labelNodes).result
        
        val cfg = new ProgramNodeFactory(astRes.rootNodes,astRes.labelNodes).result
//        writer  = new PrintWriter(basePath + "test.dot")
//        writer.write("digraph {\n%s}".format(cfg))
//        writer.close
//        
//        // generate the png image
//        Seq(dot,"-Tpng",basePath + "test.dot","-o",basePath + fileName + ".png") !
        
        cfg
    }
    
    val file = new File("ModelChecker/unitary_tests/Model_checker/main.cpp")
    val name = file.getName
    val s    = name.substring(0,name.lastIndexOf('.'))
    
    // generate the Clang AST and print it in a file
    val cfg       = buildGraph(file.getPath,s)
    val mainGraph = cfg.decls("main")
    val checker   = new ModelChecker[CFGMetaVar, ProgramNode, CFGVal](mainGraph, ProgramNode.convert)
           
//    val res = checker.evalExpr(EU(Predicate(new StatementLabelizer(VarDeclPattern(DefinedDecl("j"), "int"))),
//            Predicate(new ExpressionLabelizer(CallExprPattern(List(DefinedExpr(DeclRefExpr("", "f","","")), DefinedExpr(DeclRefExpr("", "q","",""))))))))
//
//    val res = checker.evalExpr(AU(
//            Predicate(new StatementLabelizer(VarDeclPattern(DefinedDecl("q"), "int"))),
//            Predicate(new ExpressionLabelizer(BinaryOpPattern(DefinedExpr(DeclRefExpr("", "q","","")), UndefinedVar(CFGMetaVar("X")), "=")))))

            
//    val res = checker.evalExpr(Predicate(new StatementLabelizer(VarDeclPattern(UndefinedVar(CFGMetaVar("X")), "int"))))
//    val res = checker.evalExpr(Predicate(new ExpressionLabelizer(OnePattern(UndefinedVar(CFGMetaVar("X"))))))
    
    
    
    val res = checker.evalExpr(Predicate(new UseLabelizer(CallExprPattern(List(DefinedExpr(DeclRefExpr("","", "f","","")), UndefinedVar(CFGMetaVar("X")))))))
    
//    val res = checker.evalExpr(AX(Predicate(new IfLabelizer(BinaryOpPattern(UndefinedVar(CFGMetaVar("X")),UndefinedVar(CFGMetaVar("X")), "==")))))
//    val res = checker.evalExpr(AX(Predicate(new IfLabelizer(BinaryOpPattern(UndefinedVar(CFGMetaVar("X")), 
//            UndefinedVar(CFGMetaVar("Y")), "==")))))
//    
//    val res = checker.evalExpr(AU(Predicate(new IfLabelizer(BinaryOpPattern(UndefinedVar(CFGMetaVar("X")), 
//     
    
    println("Res : " +res)
}