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
 *  
 */

package ast.test

import java.io.File
import java.io.PrintWriter
import scala.sys.process._
import ast.ASTParser
import ast.ProgramNodeFactory
import ast.SourceCodeNodeFactory
import ast.model.Program

/**
 * This class enables automated test of the AST to CFG transformation. Everything is handled from
 * the call to the Clang compiler to the creation of a png representing the graph. See the associated 
 * README for use.
 * @author Sofia Boutahar
 * @author David Courtinot
 */
object TestCFG extends App {
    /**
     * Performs the CFG generation. 
     * @param filePath path of the C++ source code file
     * @param fileName file name of the cpp file
     * @param dot path of the dot application
     * */
    def process(filePath: String, fileName: String, dot: String="dot") = {
        val cmd       = "clang -Xclang -ast-dump -std=c++11 -fsyntax-only -w " + filePath
        val basePath  = filePath.substring(0,filePath.lastIndexOf(fileName))
        val clangPath = basePath + fileName + ".txt"
        println(clangPath)
        
        var writer = new PrintWriter(clangPath)
        writer.write(cmd.!!)
        writer.close
        
        // parse the AST
        val parseResult = (new ASTParser).parseFile(clangPath)
        val astRes      = new SourceCodeNodeFactory(parseResult.root,parseResult.labels).result 

        // generate the CFG and write it in a file
        val cfg = new ProgramNodeFactory(astRes.rootNodes,astRes.labelNodes).result
        writer  = new PrintWriter(basePath + "test.dot")
        writer.write("digraph {\n%s}".format(cfg))
        writer.close
        
        // generate the png image
        Seq(dot,"-Tpng",basePath + "test.dot","-o",basePath + fileName + ".png").!
        cfg
    }
    
    val folder = "if"
    new File("ModelChecker/unitary_tests/ast/%s/".format(folder)).listFiles.filter(_.getName.endsWith("cpp")).foreach { file => 
        val name = file.getName
        val s    = name.substring(0,name.lastIndexOf('.'))
        
        // generate the Clang AST and print it in a file
        process(file.getPath,s)
    }
}