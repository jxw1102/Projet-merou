package cfg

import java.io.File
import scala.reflect.runtime.universe
import ctl._
import ast.ProgramNode

object Properties {
    type CTL = CtlExpr[CFGMetaVar,ProgramNode,CFGVal]
    
//	val INFEASIBLE_PATH  = Predicate(InfeasiblePathLabelizer())
//	val ARITH_POINTER    = Predicate(ArithmeticPointerLabelizer())
//    val UNUSED_VAR       = (Predicate(VarDeclLabelizer(VarDeclPattern(None, UndefinedVar("X")))) 
//              && AX(AG(Predicate(UnusedLabelizer(UndefinedVar("X"))))))
//    val UNUSED_FUNCTION_VALUE = Predicate(UnusedFunctionValue())
//    // Incorrect !! To debug...
//    val HIDDEN_VAR_DEF  = (Predicate(VarDefLabelizer(VarDeclPattern(None, UndefinedVar("X")))) 
//    		&& EX(EF(Predicate(VarDefLabelizer(VarDeclPattern(None, UndefinedVar("X")))))))
//    val FIND_FUNCTION = Predicate(FindExprLabelizer(CallExprPattern(UndefinedVar("X"))))
    val FIND_FUNCTION_PARAMS = Predicate(FindExprLabelizer(CallExprPattern(UndefinedVar("X"), Some(List(UndefinedVar("Y"),UndefinedVar("Z"))))))

    val FUNCTION_UNUSED_VALUE   =  {
        val nonVoidFunctionCall: CTL = Predicate(FindExprLabelizer(CallExprPattern(UndefinedVar("Z"),None,NotString(Set("void")))))
        val existsExpr         : CTL = Exists(("X",CFGExpr),Predicate(ExpressionLabelizer(UndefinedVar("X"))))
        val notAssignment      : CTL = Not(Exists(("X",NoType()),(Exists(("Y",NoType()),
                Predicate(ExpressionLabelizer(AssignmentPattern(UndefinedVar("X"),UndefinedVar("Y"))))))))
        nonVoidFunctionCall && existsExpr && notAssignment
    }
}

