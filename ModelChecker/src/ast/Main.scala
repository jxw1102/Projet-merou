package ast

import scala.reflect.io.File

import sys.process._

object Main extends App {
	val parser      = new ASTParser
	val parseResult = parser.parseFile(args(0))
//	println(parseResult.root.mkString)
//	println(parseResult.labels)
    
    var count = 0
    parseResult.root.children.foreach { x => 
        x match {
            case ConcreteASTNode(_,ofType,_,_,_) if ofType == "ParmVarDecl" => count = count + 1
            case _                                                          => 
        }        
    }
    parseResult.root.children.remove(0, count)
    val astRes = new SourceCodeNodeFactory(parseResult.root,parseResult.labels).result
    
    val cfg = new ProgramNodeFactory(astRes.rootNodes(0), astRes.labelNodes).result
    
    val gv = cfg.mkString
//    println(gv)
    val cmd = "echo '" + "digraph G {\n" + gv + "}'" + " | /usr/local/bin/dot -T png -o test.png && open test.png"
    File("test.sh").writeAll(cmd)
    Process("sh test.sh").!
}
