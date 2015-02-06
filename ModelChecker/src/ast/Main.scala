package ast

import ast.model.CompoundStmt
import ast.model.NullStmt

object Main extends App {
    val parser = new ASTParser
    val parseResult = parser.parseFile(args(0))
    //	println(parseResult.root.mkString)
    //	println(parseResult.labels)
    val astRes = new SourceCodeNodeFactory(parseResult.root /*,parseResult.labels*/ ).result

    val cfg = new ProgramNodeFactory(astRes.rootNodes(0), astRes.labelNodes).result
    println(cfg.mkString)
}