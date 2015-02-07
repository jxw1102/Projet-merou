echo 'digraph G {
CompoundStmt_102871e90 -> DeclStmt_102870cf8;
DeclStmt_102870cf8 -> DeclStmt_102870d98;
DeclStmt_102870d98 -> DeclStmt_102870f18;
DeclStmt_102870f18 -> ForStmt_102871e10;
ForStmt_102871e10 -> BinaryOp_102870f78;
BinaryOp_102870f78 -> BinaryOp_102871068;
BinaryOp_102871068 -> ForStmt_1028719a8;
BinaryOp_102871068 -> ReturnStmt_102871e70;
ForStmt_1028719a8 -> BinaryOp_102871120;
BinaryOp_102871120 -> BinaryOp_102871278;
BinaryOp_102871278 -> UnaryOp_1028710b8;
BinaryOp_102871278 -> IfStmt_102871978;
UnaryOp_1028710b8 -> BinaryOp_102871068;
IfStmt_102871978 -> BinaryOp_1028714e0;
BinaryOp_1028714e0 -> CompoundStmt_102871948;
BinaryOp_1028714e0 -> UnaryOp_1028712c8;
CompoundStmt_102871948 -> BinaryOp_1028715f0;
BinaryOp_1028715f0 -> BinaryOp_1028717c8;
BinaryOp_1028717c8 -> BinaryOp_102871920;
BinaryOp_102871920 -> UnaryOp_1028712c8;
UnaryOp_1028712c8 -> BinaryOp_102871278;
UnaryOp_1028712c8 -> BinaryOp_102871278;
}' | /usr/local/bin/dot -T png -o test.png && open test.png