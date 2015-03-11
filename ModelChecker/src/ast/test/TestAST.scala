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
 */

package ast.test

import java.io.PrintWriter
import ast.ProgramNodeFactory
import ast.ASTParser
import ast.SourceCodeNodeFactory
import scala.sys.process._
import java.io.File

/**
 * Test file for AST parsing. All the transformation steps are automated but the output require manual
 * checking. See the associated README for use.
 * @author Sofia Boutahar
 * @author David Courtinot
 */
object TestAST extends App {
    /**
     * Performs the SourceCodeNode tree construction.
     * @param filePath path of the C++ source code file
     * @param fileName file name of the cpp file
     * @param dot path of the dot application
     * */
    def process(filePath: String, fileName: String, dot: String="dot") = {
        val cmd       = "clang -Xclang -ast-dump -std=c++11 -fsyntax-only -w " + filePath
        val basePath  = filePath.substring(0,filePath.indexOf(fileName))
        val clangPath = basePath + fileName + ".txt"
        println(clangPath)
        
        // generate the Clang AST and print it in a file
        var writer = new PrintWriter(clangPath)
        writer.write(cmd.!!)
        writer.close
        
        // parse the AST
        val parseResult = (new ASTParser).parseFile(clangPath)
        println("Labels : " + parseResult.labels.mkString("\n"))
        println(parseResult.root.mkString)
    }
    
    val folder = "various"
    new File("ModelChecker/unitary_tests/ast/%s/".format(folder)).listFiles.filter(_.getName.endsWith("cpp")).foreach { file => 
         val name = file.getName
         val s    = name.substring(0,name.lastIndexOf('.'))
         process(file.getPath,s)
     }
}