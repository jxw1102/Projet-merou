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

package ctl.test

import scala.reflect.runtime.universe
import ctl.AG
import ctl.Bottom
import ctl.BottomEnv
import ctl.ConvertEnv
import ctl.MetaVariable
import ctl.True
import ctl.Value
import ctl.CtlExpr

/**
 * This file tests our implicit declarations and syntactic sugar for environments. In particular,
 * we want to assert that BottomEnv behaves as a singleton for a given [M,V] types pair.
 * @author Zohour Abouakil
 * @author David Courtinot
 */
object TestImplicit extends App with ConvertEnv {
  class MyVariable extends MetaVariable
  class MyValue    extends Value
  
  class PatateVariable extends MetaVariable
  class PatateValue    extends Value
  
  val b1 = BottomEnv.create[MyVariable, MyValue]
  val b2 = Bottom & b1
  
  val b3 = BottomEnv.create[PatateVariable, PatateValue]
  val b4 = Bottom & b3
  val b5: BottomEnv[PatateVariable, PatateValue] = Bottom
  
  val b6 = Bottom
  
  val t1: CtlExpr[MyVariable,Any,MyValue]    = True
  val t2: CtlExpr[MyVariable,String,MyValue] = True
  
  println(b1 == b2)
  println(b1 == b3)
  println(b1 == b4)
  
  println(b2 == b3)
  println(b2 == b4)
  
  println(b3 == b4)
  println(b3 == b5)
  
  println(t1 == t2)
}