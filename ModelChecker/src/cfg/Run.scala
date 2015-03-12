package run

import java.io.File
import scala.io.Source
import ast.model.ProgramNode
import cfg.CFGMetaVar
import cfg.CFGVal
import cfg.ConvertNodes
import ctl.ModelChecker
import cfg.test.Main
import cfg.parser.P
import ctl.CtlExpr

object Run {
    
    def main(args:Array[String]): Unit = {
        if (args.length < 2) {
            println("mérou cpp_file properties_file")
            return
        }
        
        val file = new File(args(0))
        val name = file.getName
        val s    = name.substring(0,name.lastIndexOf('.'))
        
        val cfg       = ast.test.TestCFG.process(file.getPath,s)
        val mainGraph = cfg.decls("main")
        
        Source.fromFile(args(1)).getLines().foreach { line =>
            val checker = new ModelChecker[CFGMetaVar, ProgramNode, CFGVal](mainGraph, ConvertNodes.convert)
            Main.printPositiveBindings(line, checker, P.calculate(line).get.asInstanceOf[CtlExpr[CFGMetaVar,ProgramNode,CFGVal]])
        }
        
    }
    
}