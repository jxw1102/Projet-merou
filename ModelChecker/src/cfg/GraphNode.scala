package cfg

import ast.ProgramNode

class GraphNode(var value: ProgramNode) {
    var next: List[GraphNode] = List()
    var prev: List[GraphNode] = List()
}