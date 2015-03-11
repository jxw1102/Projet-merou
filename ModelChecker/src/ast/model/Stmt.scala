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
 *	  - Sofia Boutahar
 *    - David Courtinot
 *    - Xiaowen Ji
 */

package ast.model

/**
 * Classes used to represent main kinds of statements within the program. They most often correspond to a specific
 * Clang class.
 * @author Sofia Boutahar
 * @author David Courtinot
 * @author Xiaowen Ji
 */
class Stmt extends SourceCodeNode 
sealed trait     JumpStmt                                                                         extends Stmt
final case class SwitchStmt    (expr: Expr, body: CompoundStmt)                                   extends Stmt
final case class CaseStmt      (condition: Expr, body: Stmt)                                      extends Stmt
final case class DefaultStmt   (body: Stmt)                                                       extends Stmt
final case class IfStmt        (condition: Expr, body: Stmt, elseStmt: Option[Stmt])              extends Stmt
final case class CompoundStmt  (elts: List[SourceCodeNode])                                       extends Stmt 
final case class LabelStmt     (label: String, stmt: Stmt)                                        extends Stmt {
    override def toString = "LabelStmt"
}
final case class NullStmt      ()                                                                 extends Stmt
final case class ContinueStmt  ()                                                                 extends JumpStmt
final case class BreakStmt     ()                                                                 extends JumpStmt
final case class GotoStmt      (label: String)                                                    extends JumpStmt
final case class ReturnStmt    (returnType: String, expr: Expr)                                   extends JumpStmt
final case class DeclStmt      (decls: List[SourceCodeNode])                                      extends ForInitializer {
    override def toString = decls.mkString(", ")
}

trait ForInitializer extends Stmt
final case class ForStmt        (init: Option[SourceCodeNode], cond: Option[Expr], update: Option[Expr], body: Stmt) extends Stmt
final case class WhileStmt      (condition: Expr, body: Stmt)                                                        extends Stmt
final case class DoWhileStmt    (condition: Expr, body: Stmt)                                                        extends Stmt
