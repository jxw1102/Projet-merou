package cfg

import ctl.EX
import ctl.Predicate
import ctl._
import ast.ProgramNode
import java.io.File

object Properties extends App {
	val UNREACHABLE_CODE = Predicate(ReturnLabelizer(Anything)) && EX(True)
	val DEAD_CODE        = Predicate(DeadIfLabelizer())
	val ARITH_POINTER    = Predicate(ArithmeticPointerLabelizer())
	
	val test = "redefinition"
	val file = new File("ModelChecker/unitary_tests/Model_checker/%s.cpp".format(test))
	val name = file.getName
	val s    = name.substring(0,name.lastIndexOf('.'))
	
	val cfg       = ast.test.TestCFG.process(file.getPath,s)
	val mainGraph = cfg.decls("main")
	val checker   = new ModelChecker[CFGMetaVar, ProgramNode, CFGVal](mainGraph, ProgramNode.convert)
	
//	println(checker.evalExpr(UNREACHABLE_CODE))
//	println(checker.evalExpr(DEAD_CODE))
	println(checker.evalExpr(ARITH_POINTER))
}

