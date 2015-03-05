package cfg

import scala.reflect.runtime.universe

import ast.model.ProgramNode
import ctl._

/**
 * This class contains some pre-defined properties constructed with basic labelizers. It includes some
 * of the MISRAC++ rules.
 * @author Zohour Abouakil
 * @author David Courtinot
 */

object Properties {
    type CTL = CtlExpr[CFGMetaVar,ProgramNode,CFGVal]

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
    
    /**
     * Detects all function call returning a value that is unused. Any expression which is not stored or used
     * for a test is considered unused. This property will return all the unused expressions containing a function
     * call.
     */
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
    val HIDDEN_VAR_DEF = Predicate(VarDefLabelizer(VarDefPattern(NotString(),UndefinedVar("X")))) &&
    		EX(EF(Predicate(VarDefLabelizer(VarDefPattern(NotString(),UndefinedVar("X"))))))
    		
    /**
     * This property detects all the assignments in the CFG. It includes the = assignment as wel as compound assignment
     * operators (+=, *=, -=, /=).
     */
    val ASSIGNMENT = Predicate(MatchExprLabelizer(AssignmentPattern(UndefinedVar("X"),UndefinedVar("Y"))))
    
    /**
     * This property detects all the nodes holding a literal expression. A literal expression is an expression which
     * all leaves are literals.
     */
    val LITERAL_EXPR = Predicate(MatchExprLabelizer(LiteralExprPattern(UndefinedVar("X"))))
    
    /**
     * This property detects all the assignments of a literal expression to any variable.
     */
    val LITERAL_ASSIGNMENT = Predicate(MatchExprLabelizer(AssignmentPattern(UndefinedVar("X"),LiteralExprPattern(UndefinedVar("Y")))))
    
    /**
     * This property detects any arithmetic expression involving a pointer type. Only assignments and increment/decrement operators
     * are allowed.
     */
    val ARITHMETIC_POINTER = {
        val pointerExpr        = Predicate(FindExprLabelizer(PointerExperPattern    (UndefinedVar("Z"))))
        val compoundAssign     = Predicate(FindExprLabelizer(CompoundAssignOpPattern(UndefinedVar("X"),UndefinedVar("Y"))))
        val arithmeticBinaryOp = Predicate(FindExprLabelizer(BinaryOpPattern        (UndefinedVar("X"),UndefinedVar("Y"),NotString("="))))
        val arithmeticUnaryOp  = Predicate(FindExprLabelizer(UnaryOpPattern         (UndefinedVar("X"),NotString("--","++"))))
        pointerExpr && (compoundAssign || arithmeticBinaryOp || arithmeticUnaryOp)
    }
    
    /**
     * This property detects all the flow-control nodes which condition is evaluated to the same value on every execution path.
     */
    val INFEASIBLE_PATH = {
        val literalAssignmentPattern = AssignmentPattern(UndefinedVar("X"),LiteralExprPattern(UndefinedVar("Y")))
        val literalExprPattern       = LiteralExprPattern(UndefinedVar("X"))
        val identityPattern          = BinaryOpPattern(UndefinedVar("X"),UndefinedVar("X"),DefinedString("=="))
        anyFlowControlNodes(literalAssignmentPattern) || anyFlowControlNodes(literalExprPattern) || 
        anyFlowControlNodes(identityPattern)          || Predicate(ForLabelizer(None))
    }
    
    /**
     * This property detects all unused variables.
     */
    val UNUSED_DECLARED_VAR = {
        val declaredVariable = Predicate(VarDeclLabelizer(VarDeclPattern(NotString(),UndefinedVar("X"))))
        val usedVariable     = Predicate(UseLabelizer(UndefinedVar("X")))
        declaredVariable && AG(!usedVariable)
    }
    
    /**
     * This property detects all functions that should be used in pairs.
     */
    val NON_PAIRED_FUNCTION_CALL = (f1: String, f2: String) => {
        val fun1       = Predicate(FindExprLabelizer(CallExprPattern(DefinedString(f1))))
        val assignment = Predicate(MatchExprLabelizer(AssignmentPattern(UndefinedVar("X"),UndefinedVar("Y"),DefinedString("="))))
        val fun2       = Predicate(FindExprLabelizer(CallExprPattern(DefinedString(f2),Some(List(UndefinedVar("X"))))))
        ((!assignment && fun1) || (fun1 && assignment && EX(EG(!fun2)))) && !(fun1 && fun2)
    }
    
    /**
     * This property detects all operations which will cause a memory leak.
     */
    val NEW_WITHOUT_DELETE = {
        val alloc      = Predicate(FindExprLabelizer(CXXNewExprPattern()))
        val assignment = Predicate(MatchExprLabelizer(AssignmentPattern(UndefinedVar("X"),UndefinedVar("Y"),DefinedString("="))))
        val dealloc    = Predicate(FindExprLabelizer(CXXDeleteExprPattern(Some(UndefinedVar("X")))))
        ((!assignment && alloc) || (alloc && assignment && EX(EG(!dealloc)))) && !(alloc && dealloc)
    }
    
}