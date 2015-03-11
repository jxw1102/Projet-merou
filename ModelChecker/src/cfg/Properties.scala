/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 * 
 *	Author(s) :
 *	  - Zohour Abouakil
 *    - David Courtinot
 */

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
    
    // factorized code for CLOSED_RESOURCES and FREED_MEMORY
    private val MEMORY_LEAK = (f1: String, f2: String) => {
        val fun1       = Predicate(FindExprLabelizer(CallExprPattern(DefinedString(f1))))
        val assignment = Predicate(MatchExprLabelizer(AssignmentPattern(UndefinedVar("X"),UndefinedVar("Y"),DefinedString("="))))
        val fun2       = Predicate(FindExprLabelizer(CallExprPattern(DefinedString(f2),Some(List(UndefinedVar("X"))))))
        (!assignment && fun1 && !fun2) || (fun1 && assignment && EX(EG(!fun2)))
    }
    
    /**
     * This property detects the files that are opened and never closed on a particular execution path
     */
    val CLOSED_RESOURCES = MEMORY_LEAK("fopen" ,"fclose")
    
    /**
     * This property detects the variables that are allocated dynamically with malloc but never released on a particular execution path
     */
    val FREED_MEMORY     = MEMORY_LEAK("malloc","free"  )
    
    /**
     * This property detects all operations which will cause a memory leak.
     */
    val NEW_WITHOUT_DELETE = {
        val alloc      = Predicate(FindExprLabelizer(CXXNewExprPattern()))
        val assignment = Predicate(MatchExprLabelizer(AssignmentPattern(UndefinedVar("X"),UndefinedVar("Y"),DefinedString("="))))
        val dealloc    = Predicate(FindExprLabelizer(CXXDeleteExprPattern(Some(UndefinedVar("X")))))
        (!assignment && alloc && !dealloc) || (alloc && assignment && EX(EG(!dealloc)))
    }
}