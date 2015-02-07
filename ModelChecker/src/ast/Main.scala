package ast

import scala.reflect.io.File

import sys.process._

object Main extends App {
	val parser      = new ASTParser
	val parseResult = parser.parseFile(args(0))
//	println(parseResult.root.mkString)
//	println(parseResult.labels)
    
    parseResult.root.children.remove(0,2)
    val astRes = new SourceCodeNodeFactory(parseResult.root,parseResult.labels).result
    
    val cfg = new ProgramNodeFactory(astRes.rootNodes(0), astRes.labelNodes).result
    val gv = cfg.mkString
//    println(gv)
    val cmd = "echo '" + "digraph G {\n" + gv + "}'" + " | /usr/local/bin/dot -T png -o test.png && open test.png"
    File("test.sh").writeAll(cmd)
    Process("sh test.sh").!
}
