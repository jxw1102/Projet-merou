package ctl

abstract class CtlExpr 

// Binary Expression
case class And (left : CtlExpr, right : CtlExpr)
case class Or  (left : CtlExpr, right : CtlExpr)
case class AU  (left : CtlExpr, right : CtlExpr)
case class EU  (left : CtlExpr, right : CtlExpr)

// Unary Expression
case class AX  (right : CtlExpr)
case class EX  (right : CtlExpr)
case class AG  (right : CtlExpr)
case class EG  (right : CtlExpr)
case class AF  (right : CtlExpr)
case class EF  (right : CtlExpr)
case class Not (right : CtlExpr)

// 

