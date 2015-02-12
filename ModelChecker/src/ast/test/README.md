# Tests - AST to CFG

## TestAST.scala

This class tests the parsing of the Clang AST by simply printing the tree of ASTNode(s) we build from the file.
It takes one or several cpp file(s) as an input and generates the AST in a .txt file with the same name as
the original file. The result is printed on the standard output.

### Requirements

- Clang compiler

## TestCFG.scala

This class tests the whole transformation chain from the Clang AST to the CFG. It creates AST files like TestAST
as well as a temporary test.dot file containing the CFG in textual format. Finally, it generates a graphic preview
of the CFG using dot

### Requirements

- Clang compiler
- dot command must be in your PATH. Get dot : http://www.graphviz.org/Download..php)

## Usage

Both test classes work the same way :

- define the test case(s) in a folder of ModelChecker/unitary_tests/
- set the 'folder' variable to the folder that contains the input cpp code you want to use for the tests
- run the test
- check he output in the folder containing the input
