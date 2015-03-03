package cfg.test

import ctl.ModelChecker
import cfg.ConvertNodes
import ctl.CtlExpr
import java.io.File
import ast.ProgramNode
import cfg.CFGMetaVar
import cfg.CFGVal
import cfg.Properties._

/**
 * This class enables to test the properties defined in cfg.Properties
 * @author Zohour Abouakil
 * @author David Courtinot
 */
object Main extends App {
	type CTL     = CtlExpr[CFGMetaVar,ProgramNode,CFGVal]
	type Checker = ModelChecker[CFGMetaVar,ProgramNode,CFGVal]
	
	def printTest(msg: String, checker: Checker, test: CTL) = println(msg + checker.evalExpr(test).mkString("\n\t","\n\t","\n"))
	def loadChecker(testName: String) = {
		val file = new File("ModelChecker/unitary_tests/Model_checker/%s.cpp".format(testName))
		val name = file.getName
		val s    = name.substring(0,name.lastIndexOf('.'))
		
		val cfg       = ast.test.TestCFG.process(file.getPath,s)
		val mainGraph = cfg.decls("main")
		new ModelChecker[CFGMetaVar, ProgramNode, CFGVal](mainGraph, ConvertNodes.convert)	    
	}
	
	lazy val checker1 = loadChecker("unused_function_value")
	lazy val test1 = {
		println("Testing the FUNCTION_UNUSED_VALUE property...")
		printTest("Following lines contain unused function returned value :",checker1,FUNCTION_UNUSED_VALUE)
	}
	     
	lazy val checker2 = loadChecker("assignments")
	lazy val test2 = {
		println("Testing the AssignmentPattern...")
		printTest("Following lines are an assignment :",checker2,ASSIGNMENT)
	}
	
	lazy val test3 = {
		println("Testing the LITERAL_ASSIGNMENT property...")
		printTest("Following lines contain a literal assignment :",checker2,LITERAL_ASSIGNMENT)
	}
	
	lazy val test4 = {
		println("Testing the LITERAL_EXPR property...")
		printTest("Following lines contain are a literal expr :",checker2,LITERAL_EXPR)
	}
	
	lazy val checker3 = loadChecker("dead_code")
	lazy val test5 = {
		println("Testing the INFEASIBLE_PATH property...")
		printTest("Following lines are flow-control nodes which will be evaluated to the same value in every execution path :",
				checker3,INFEASIBLE_PATH)
	}
	
	lazy val checker4 = loadChecker("hidden_var_def")
	lazy val test6 = {
		println("Testing the HIDDEN_VAR_DEF property...")
		printTest("Following lines are variable definitions that are hidden later in the code (may contain false positive results) :",
				checker4,HIDDEN_VAR_DEF)
	}
	
	lazy val checker5 = loadChecker("arith_pointer")
	lazy val test7 = {
		println("Testing the ARITHMETIC_POINTER property...")
		printTest("Following lines contain an arithmetic expression involving a pointer :",checker5,ARITHMETIC_POINTER)
	}
	
    lazy val checker6 = loadChecker("unused_var")
    lazy val test8 = {
        println("Testing the UNUSED_DECALRED_VAR property...")
        printTest("Following lines contain variable definition that are not used :",checker6,UNUSED_DECALRED_VAR)
    }
	test6
}