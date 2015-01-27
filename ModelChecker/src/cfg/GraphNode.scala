package cfg

<<<<<<< HEAD
class GraphNode
=======
import ast.ProgramNode

class GraphNode(var value: ProgramNode) {
    var next: List[GraphNode] = List()
    var prev: List[GraphNode] = List()
}
>>>>>>> branch 'master' of https://github.com/jxw1102/Projet-merou.git
