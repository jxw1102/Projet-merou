package ast

import ast.model.CompoundStmt

object Main extends App {
	val parser      = new ASTParser
	val parseResult = parser.parseFile(args(0))
//	println(parseResult.root.mkString)
//	println(parseResult.jumps)
    val ast = new SourceCodeNodeFactory(parseResult.root).rootNodes
    println(ast(0).asInstanceOf[CompoundStmt].elts.mkString("\n"))
}