package ast

import java.io.File
import java.io.PrintWriter

import scala.sys.process._

/**
 * This class enables automated test of the AST to CFG transformation. Everything is handled from
 * the call to the Clang compiler to the creation of a png representing the graph.
 * @author David Courtinot
 * @author Sofia Boutahar
 */
object Main extends App {
    
    def process(filePath: String, fileName: String, dot: String="dot") = {
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
        val cfg = new ProgramNodeFactory(astRes.rootNodes(0),astRes.labelNodes).result
        writer  = new PrintWriter(basePath + "test.dot")
        writer.write("digraph {\n%s}".format(cfg.mkString))
        writer.close
        
        // generate the png image
        Seq(dot,"-Tpng",basePath + "test.dot","-o",basePath + fileName + ".png") !
    }
    
    if (args.length > 0) {
        process("./"+args(0),"test","/usr/local/bin/dot")
        "open test.png".!!
    } else {
        val folder = "switch"
        new File("unitary_tests/%s/".format(folder)).listFiles.filter(_.getName.endsWith("cpp")).foreach { file => 
            val name = file.getName
            val s    = name.substring(0,name.lastIndexOf('.'))
            
            // generate the Clang AST and print it in a file
            process(file.getPath,s)
        }
    }
    
}