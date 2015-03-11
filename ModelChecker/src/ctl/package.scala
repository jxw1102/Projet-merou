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

import scala.reflect.runtime.universe._

package object ctl {
	def AF[M <: MetaVariable: TypeTag, N: TypeTag, V <: Value: TypeTag](p: CtlExpr[M,N,V]): CtlExpr[M,N,V] = AU(True,p)
	def EF[M <: MetaVariable: TypeTag, N: TypeTag, V <: Value: TypeTag](p: CtlExpr[M,N,V]): CtlExpr[M,N,V] = EU(True,p)
	def AG[M <: MetaVariable: TypeTag, N: TypeTag, V <: Value: TypeTag](p: CtlExpr[M,N,V]): CtlExpr[M,N,V] = Not(EF(Not(p)))
	def EG[M <: MetaVariable: TypeTag, N: TypeTag, V <: Value: TypeTag](p: CtlExpr[M,N,V]): CtlExpr[M,N,V] = Not(AF(Not(p)))
	    
	implicit def trueTotrue  [M <: MetaVariable: TypeTag, N: TypeTag, V <: Value: TypeTag](t: True) : Predicate[M,N,V] = 
        Predicate(Labelizer.createTrue[M,N,V])
        
    implicit def falseTofalse[M <: MetaVariable: TypeTag, N: TypeTag, V <: Value: TypeTag](t: False): Predicate[M,N,V] = 
        Predicate(Labelizer.createFalse[M,N,V])
}