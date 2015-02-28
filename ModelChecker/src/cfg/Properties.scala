package cfg

import scala.reflect.runtime.universe

import ast.ProgramNode
import ctl._

/**
 * This class contains some pre-defined properties constructed with basic labelizers. It includes some
 * of the MISRAC++ rules.
 * @author Zohour Abouakil
 * @author David Courtinot
 */
object Properties {
    type CTL = CtlExpr[CFGMetaVar,ProgramNode,CFGVal]
    
//    val UNUSED_VAR       = (Predicate(VarDeclLabelizer(VarDeclPattern(None, UndefinedVar("X")))) 
//              && AX(AG(Predicate(UnusedLabelizer(UndefinedVar("X"))))))
    
    /**
     * Macro predicate to compute the conjunction of pattern-based predicates for each If, While, For,
     * and Switch node
     */
    def forallFlowControlNodes(pattern: ExprPattern) = 
    	Predicate(IfLabelizer    (pattern)) && Predicate(WhileLabelizer (pattern))    &&
        Predicate(SwitchLabelizer(pattern)) && Predicate(ForLabelizer(Some(pattern))) 
        
	/**
	* Macro predicate to compute the disjunction of a pattern-based predicate for each If, While, For,
	* and Switch node
	*/
    def anyFlowControlNodes(pattern: ExprPattern) = 
    	Predicate(IfLabelizer    (pattern)) || Predicate(WhileLabelizer (pattern))    ||
        Predicate(SwitchLabelizer(pattern)) || Predicate(ForLabelizer(Some(pattern))) 
    
    val FIND_FUNCTION_PARAMS = Predicate(FindExprLabelizer(CallExprPattern(UndefinedVar("X"), Some(List(UndefinedVar("Y"),UndefinedVar("Z"))))))

    val FUNCTION_UNUSED_VALUE =  {
        val nonVoidFunctionCall: CTL = Predicate(FindExprLabelizer(CallExprPattern(UndefinedVar("Z"),None,NotString("void"))))
        val existsExpr         : CTL = Exists(("X",CFGExpr),Predicate(ExpressionLabelizer(UndefinedVar("X"))))
        val notAssignment      : CTL = Not(Exists(("X",CFGExpr),(Exists(("Y",CFGExpr),
                Predicate(ExpressionLabelizer(AssignmentPattern(UndefinedVar("X"),UndefinedVar("Y"))))))))
        nonVoidFunctionCall && existsExpr && notAssignment
    }
    
    /**
     * Detects all hidden variable definitions. May also return false positive results in cases such as
     * 
     * { int x; }
     * int x;
     * 
     * Require manual checking. More generally, it detects the bad practice of giving the same name to
     * variables of the same type in different scopes.
     */
    val HIDDEN_VAR_DEF     = Predicate(VarDefLabelizer(VarDefPattern(NotString(),UndefinedVar("X")))) &&
    		EX(EF(Predicate(VarDefLabelizer(VarDefPattern(NotString(),UndefinedVar("X"))))))
    val ASSIGNMENT         = Predicate(MatchExprLabelizer(AssignmentPattern(UndefinedVar("X"),UndefinedVar("Y"))))
    val LITERAL_EXPR       = Predicate(MatchExprLabelizer(LiteralExprPattern(UndefinedVar("X"))))
    val LITERAL_ASSIGNMENT = Predicate(MatchExprLabelizer(AssignmentPattern(UndefinedVar("X"),LiteralExprPattern(UndefinedVar("Y")))))
    val ARITHMETIC_POINTER = {
        val pointerExpr        = Predicate(FindExprLabelizer(PointerExperPattern    (UndefinedVar("Z"))))
        val compoundAssign     = Predicate(FindExprLabelizer(CompoundAssignOpPattern(UndefinedVar("X"),UndefinedVar("Y"))))
        val arithmeticBinaryOp = Predicate(FindExprLabelizer(BinaryOpPattern        (UndefinedVar("X"),UndefinedVar("Y"),NotString("="))))
        val arithmeticUnaryOp  = Predicate(FindExprLabelizer(UnaryOpPattern         (UndefinedVar("X"),NotString("--","++"))))
        pointerExpr && (compoundAssign || arithmeticBinaryOp || arithmeticUnaryOp)
    }
    val INFEASIBLE_PATH    = {
        val literalAssignmentPattern = AssignmentPattern(UndefinedVar("X"),LiteralExprPattern(UndefinedVar("Y")))
        val literalExprPattern       = LiteralExprPattern(UndefinedVar("X"))
        val identityPattern          = BinaryOpPattern(UndefinedVar("X"),UndefinedVar("X"),DefinedString("=="))
        anyFlowControlNodes(literalAssignmentPattern) || anyFlowControlNodes(literalExprPattern) || 
        anyFlowControlNodes(identityPattern)          || Predicate(ForLabelizer(None))
    }
}