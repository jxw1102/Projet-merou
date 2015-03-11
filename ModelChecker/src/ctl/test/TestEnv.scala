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
 *    - Xiaowen Ji
 */

package ctl.test

import ctl._
import scala.reflect.runtime.universe

/**
 * This class contains automated tests for the environments classes. It basically consists in unitary tests
 * of each elementary operation implemented by an Environment.
 * @author Zohour Abouakil
 * @author Xiaowen Ji
 * @author David Courtinot
 */
object TestEnv extends App with TestUtils with ConvertEnv {
	type Binding = BindingsEnv[Identifier,Identifier]
    type Env     = Environment[Identifier,Identifier]
    
    implicit def posTuple(s: (String,String)): (Identifier,Identifier) = Identifier(s._1) -> Identifier(s._2)
    implicit def negTuple(s: (String,Set[String])): (Identifier,Set[Identifier]) =
        Identifier(s._1) -> s._2.map(Identifier(_))
        
    def compareEnv  (envT1: Env, envT2: Env, expected: Env) = assertEquals(envT1 & envT2,expected)
    def testNeg     (env: Env, envs: Env*)                  = assertEquals(!env,Set(envs: _*))
        
    println("Testing environments...\n-----------------------")
    println("Testing compareEnv...")
    // test 0
     val env1 = new Binding ++ ("X" -> "2","Z" -> "3")
     val env2 = new Binding ++ ("Y" -> "4")
    compareEnv(env1,env2,new Binding ++ ("X" -> "2","Y" -> "4","Z" -> "3"))
    
    // test 1
    val env3 = new Binding ++ ("X" -> "7")
    val env4 = new Binding ++ ("X" -> "5")
    compareEnv(env3,env4,Bottom)
    
    // test 2
    val env5 = new Binding ++ ("X" -> "2")
    val env6 = new Binding -- ("X" -> Set("1", "2"))
    compareEnv(env5,env6,Bottom) 
    
    // test 3
    val env7 = new Binding ++ ("X" -> "15")
    val env8 = (new Binding ++ ("Y" -> "15")) -- ("X" -> Set("1", "3"))
    compareEnv(env7,env8,new Binding ++ ("X" -> "15","Y" -> "15")) 
    
    // test 4
    val env9  = new Binding -- ("X" -> Set("1", "3"))
    val env10 = new Binding -- ("X" -> Set("5", "6"))
    compareEnv(env9,env10,new Binding -- ("X" -> Set("1", "3","5","6")))
    
    println("\nTesting the negation of an environment...")
    // test 5
    testNeg(Bottom,new Binding)
    
    // test 6
    testNeg(env1,new Binding -- ("X" -> Set("2")),new Binding -- ("Z" -> Set("3")))
    
    // test 7
    testNeg(env6,new Binding ++ ("X" -> "1"),new Binding ++ ("X" -> "2"))
    
    // test 8
    testNeg(env8,new Binding ++ ("X" -> "1"),new Binding ++ ("X" -> "3"),new Binding -- ("Y" -> Set("15")))
    
    println("\nTesting '-' operation...")
    // test 9
    assertEquals((env1 & env2) - "Z",new Binding ++ ("X" -> "2","Y" -> "4"))
}