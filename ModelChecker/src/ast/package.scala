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
 *    - Xiaowen Ji
 */

/**
 * This package contains all the classes needed to model the Clang AST (well, only a small
 * part of it but it is sufficient to play with purely imperative C++).
 */
package object ast {
    /**
     * Enables to split the 'data' field of the ConcreteASTNode(s)
     * */
    implicit class DataProcessor(data: String) {
        val dataList = DataProcessor.splitReg.findAllIn(data).map(_.replaceAll("'", "")).toSeq
    }
    
    /**
     * Separates a data string into several fragments
     * */
    implicit object DataProcessor {
        private val splitReg = "<[^>]+>+|\\(.+\\)|(\"[^\"]+\")|('[^']+'|[^\\s']+)+".r
    }
    
    /**
     * Enables negative indexes for look-up on a Seq
     * */
    implicit class SeqFetcher[T](seq: Seq[T]) {
        def get(idx: Int) = if (idx >= 0) seq(idx) else seq(seq.length + idx)
    }
}