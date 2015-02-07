echo 'digraph G {
CompoundStmt_102871048 -> DeclStmt_102870d88;
DeclStmt_102870d88 -> DeclStmt_102870ff0;
DeclStmt_102870ff0 -> ReturnStmt_102871028;
}' | /usr/local/bin/dot -T png -o test.png && open test.png