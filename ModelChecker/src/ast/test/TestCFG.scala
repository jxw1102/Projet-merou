package ast.test

import java.io.File
import java.io.PrintWriter
import scala.sys.process._
import ast.ASTParser
import ast.ProgramNodeFactory
import ast.SourceCodeNodeFactory
import ast.Program

/**
 * This class enables automated test of the AST to CFG transformation. Everything is handled from
 * the call to the Clang compiler to the creation of a png representing the graph. See the associated 
 * README for use.
 * @author Sofia Boutahar
 * @author David Courtinot
 */
object TestCFG extends App {
    def process(filePath: String, fileName: String, dot: String="dot") = {
        val cmd       = "clang -Xclang -ast-dump -std=c++11 -fsyntax-only -w " + filePath
        val basePath  = filePath.substring(0,filePath.lastIndexOf(fileName))
        val clangPath = basePath + fileName + ".txt"
        println(clangPath)
        
        var writer = new PrintWriter(clangPath)
        writer.write(cmd.!!)
        writer.close
        
        // parse the AST
        val parseResult = (new ASTParser).parseFile(clangPath)
        val astRes      = new SourceCodeNodeFactory(parseResult.root,parseResult.labels).result 

        // generate the CFG and write it in a file
        val cfg = new ProgramNodeFactory(astRes.rootNodes,astRes.labelNodes).result
        writer  = new PrintWriter(basePath + "test.dot")
        writer.write("digraph {\n%s}".format(cfg))
        writer.close
        
        // generate the png image
        Seq(dot,"-Tpng",basePath + "test.dot","-o",basePath + fileName + ".png").!
        cfg
    }
    
    val folder = "for"
    new File("ModelChecker/unitary_tests/ast/%s/".format(folder)).listFiles.filter(_.getName.endsWith("cpp")).foreach { file => 
        val name = file.getName
        val s    = name.substring(0,name.lastIndexOf('.'))
        
        // generate the Clang AST and print it in a file
        process(file.getPath,s)
    }
}