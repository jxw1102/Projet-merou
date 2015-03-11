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
 * Classes used to represent main kinds of declarations within the program. They most often correspond to a specific
 * Clang class.
 * @author Sofia Boutahar
 * @author David Courtinot
 * @author Xiaowen Ji
 */
abstract class Decl(_name: String, _typeOf: String) extends SourceCodeNode {
    def name   = _name
    def typeOf = _typeOf
}

final case class VarDecl(_name: String, typeName: String, value: Option[Expr]) extends Decl(_name,typeName) {
    override def toString = "%s %s %s".format(typeName,name,if (value.isDefined) " = " + value.get else "")
}

final case class FunctionDecl(_name: String, typeName: String, args: List[ParamVarDecl], body: CompoundStmt) extends Decl(_name,typeName) {
    override def toString = "%s %s(%s)".format(typeName,name,args.mkString(", "))
}

final case class ParamVarDecl(_name: String, typeName: String) extends Decl(_name,typeName) {
    override def toString = "%s %s".format(typeName,name)
}

final case class TypedefDecl(alias: String, original: String) extends Decl(alias,original) {
    override def toString = "typedef %s %s".format(original,alias)
}

final case class EnumDecl(typeName: String) extends Decl(typeName,typeName) {
    override def toString = "enum %s".format(typeName)
}

final case class EnumConstantDecl(_name: String, typeName: String) extends Decl(_name,typeName) {
    override def toString = "%s %s".format(typeName,name)
}

final case class FieldDecl(_name: String, typeName: String) extends Decl(_name,typeName) {
    override def toString = "%s %s".format(typeName,name)
}
