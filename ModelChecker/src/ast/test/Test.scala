//package ast.test
//
//import sys.process._
//import java.io.File
//import ast.ASTParser
//import ast.SourceCodeNodeFactory
//import ast.ProgramNodeFactory
//import java.io.PrintWriter
//object Test extends App {
//    val cppFilePath = "/Users/sofiaboutahar/Documents/workspace/Projet-merou/CppCode/For.cpp"
//    val cppFile = new File(cppFilePath)
//    val name = cppFile.getName.split(".cpp")(0)
//    var clangCmd = "clang -Xclang -ast-dump -fsyntax-only " + cppFilePath
//    val clangFilePath = cppFile.getParent + "/" + name + ".txt"
//    val writer = new PrintWriter(clangFilePath)
//    writer.write(clangCmd.!!)
//    writer.close()
//    println(clangCmd.!!)
//    val parser = new ASTParser
//    val parseResult = parser.parseFile(clangFilePath)
//    val astResult = new SourceCodeNodeFactory(parseResult.root,parseResult .labels).result
//    val cfg = new ProgramNodeFactory(astResult.rootNodes(0), astResult.labelNodes).result
//
//    val digraph = "Digraph {\n" + cfg.mkString + "}"
//    println(digraph)
//    val format = "png"
//    val graphPath = cppFile.getParent;
//    generateGraph("tmp.dot", digraph, graphPath, name, format)
//
//    def generateGraph(dotFilePath: String, digraph: String, graphPath: String, name: String, format: String) {
//        val dotFile = new File(dotFilePath)
//        val writer = new PrintWriter(dotFile)
//        writer.write(digraph)
//        writer.close()
//        var dotCmd = "dot -T";
//        format match {
//            case "pdf" =>
//                dotCmd += "pdf " + dotFilePath
//                val psFile = new File("tmp.ps")
//                (dotCmd #> psFile).!
//                ("ps2pdf tmp.ps " + graphPath + name + ".pdf").!
//                psFile.delete()
//
//            case "png" | _ =>
//                dotCmd += "png " + dotFilePath
//                (dotCmd #> graphPath + name + ".png").!
//        }
//    }
//}