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


/**
 * @author Zohour Abouakil 
 * @author David Courtinot
 */
object Main extends App {
    def buildGraph(filePath: String, fileName: String) = {
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
        new ProgramNodeFactory(astRes.rootNodes,astRes.labelNodes).result
    }
    
    val folder = "various"
        new File("ModelChecker/unitary_tests/%s/".format(folder)).listFiles.filter(_.getName.endsWith("cpp")).foreach { file => 
            val name = file.getName
            val s    = name.substring(0,name.lastIndexOf('.'))
            
            // generate the Clang AST and print it in a file
            val cfg = buildGraph(file.getPath,s)
            
            val mainGraph = cfg.decls("main")
            
            val checker = new ModelChecker[CfgMetaVar, ProgramNode, CFGVal](mainGraph, ProgramNode.convert)
            
            //checker.evalExpr(AX[CfgMetaVar, ProgramNode, CFGVal](PredicateUndefinedExpr(CfgMetaVar("X"))))
        }
    
}