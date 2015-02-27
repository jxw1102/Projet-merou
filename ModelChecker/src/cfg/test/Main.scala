package cfg.test

import ctl.ModelChecker
import cfg.ConvertNodes
import ctl.CtlExpr
import java.io.File
import ast.ProgramNode
import cfg.CFGMetaVar
import cfg.CFGVal
import cfg.Properties

object Main extends App {
	type CTL = CtlExpr[CFGMetaVar,ProgramNode,CFGVal]
	
	def loadChecker(testName: String) = {
		val file = new File("ModelChecker/unitary_tests/Model_checker/%s.cpp".format(testName))
		val name = file.getName
		val s    = name.substring(0,name.lastIndexOf('.'))
		
		val cfg       = ast.test.TestCFG.process(file.getPath,s)
		val mainGraph = cfg.decls("main")
		new ModelChecker[CFGMetaVar, ProgramNode, CFGVal](mainGraph, ConvertNodes.convert)	    
	}
	
	println("Testing the FUNCTION_UNUSED_VALUE property...")
	val checker1 = loadChecker("unused_function_value")
	println("Following lines contain unused function returned value :" + 
	        checker1.evalExpr(Properties.FUNCTION_UNUSED_VALUE).mkString("\n\t","\n\t",""))
	
//	println(checker.evalExpr(UNREACHABLE_CODE))
//	println(checker.evalExpr(INFEASIBLE_PATH))
//	println(checker.evalExpr(ARITH_POINTER))
//	println(checker.evalExpr(UNUSED_FUNCTION_VALUE))
//	println(checker.evalExpr(HIDDEN_VAR_DEF))
	
//    println(checker.evalExpr(UNUSED_VAR))
//    println("\ndecl : " + checker.evalExpr(Predicate(VarDeclLabelizer(VarDeclPattern(None, UndefinedVar("X"))))))
//    println("\nunused : " + checker.evalExpr(EF(AX(Predicate(UnusedLabelizer(UndefinedVar("X")))))))
//    println(checker.evalExpr(Predicate(VarDeclLabelizer(VarDeclPattern(None, UndefinedVar("X")))) &&
//            AX(Not(EF(Not(Predicate(UnusedLabelizer(UndefinedVar("X")))))))))
//	println(checker.evalExpr(ARITH_POINTER))
//  println(checker.evalExpr(UNUSED_VAR))
//  println(checker.evalExpr(REDEFINE_VAR))
//    println(checker.evalExpr(FIND_FUNCTION_PARAMS).size)

}