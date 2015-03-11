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
 *    - David Courtinot
 */

package ctl.test

import ctl.MetaVariable
import ctl.Value

/**
 * Utility class for automated testing
 * @author David Courtinot
 */
trait TestUtils {
	private var i = 0
    def printMsg(failed: Boolean)     = { 
	    val msg = "\tTest %d %s".format(i,if (failed) "failed" else "passed") 
	    if (failed) Console.err.println(msg)
	    else        println(msg)
	    i += 1
	}
    def assertEquals[T](a0: T, a1: T) = printMsg(a0 != a1)
    def assertTrue     (b: Boolean)   = printMsg(!b)
    
    implicit def strToId(s: String): Identifier = Identifier(s)
    
    def newTestSession = i = 0
}

case class Identifier(name: String) extends Value with MetaVariable {
    override def hashCode       = name.hashCode
    override def equals(a: Any) = a match {
        case Identifier(value) => value == name
        case _                 => false 
    }
    override def toString       = name
}