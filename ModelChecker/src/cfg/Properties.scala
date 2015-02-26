package cfg

import ctl.EX
import ctl.Predicate
import ctl._
import ast.ProgramNode
import java.io.File

object Properties extends App {
    implicit def strToMeta(s: String): CFGMetaVar = CFGMetaVar(s)
    
	val UNREACHABLE_CODE = Predicate(ReturnLabelizer(UndefinedVar("X"))) && EX(True)
	val DEAD_CODE        = Predicate(DeadIfLabelizer())
	val ARITH_POINTER    = Predicate(ArithmeticPointerLabelizer())
    val UNUSED_VAR       = (Predicate(VarDeclLabelizer(VarDeclPattern(None, UndefinedVar("X")))) 
              && AX(AG(Not(Predicate(UnusedLabelizer(UndefinedVar("X")))))))
    val REDEFINE_VAR      = (Predicate(VarDefLabelizer(VarDeclPattern(None, UndefinedVar("X")))) 
              && EX(EF(Predicate(VarDefLabelizer(VarDeclPattern(None, UndefinedVar("X")))))))
	
	val test = "dead_code"
	val file = new File("ModelChecker/unitary_tests/Model_checker/%s.cpp".format(test))
	val name = file.getName
	val s    = name.substring(0,name.lastIndexOf('.'))
	
	val cfg       = ast.test.TestCFG.process(file.getPath,s)
	val mainGraph = cfg.decls("main")
	val checker   = new ModelChecker[CFGMetaVar, ProgramNode, CFGVal](mainGraph, ConvertNodes.convert)
	
//	println(checker.evalExpr(UNREACHABLE_CODE))
	println(checker.evalExpr(DEAD_CODE))
//	println(checker.evalExpr(ARITH_POINTER))
//  println(checker.evalExpr(UNUSED_VAR))
//  println(checker.evalExpr(REDEFINE_VAR))
}

