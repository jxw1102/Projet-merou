package cfg

import ast.ProgramNode

class GraphNode(val value: ProgramNode) {
    var next: List[GraphNode] = List()
    var prev: List[GraphNode] = List()
}