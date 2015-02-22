package ctl.parser

import scala.io.Source
import scala.util.parsing.combinator._
import ast.model.BinaryOp
import ast.model.Expr
import ast.model.DeclRefExpr
import ast.model.Literal
import ast.model.CallExpr
import ast.model.UnaryOp
import ast.model.OpPosition
import ast.model.CompoundAssignOp
import ast.model.DeclRefExpr
/**
 * @author sofiaboutahar
 */
object CTLInterpreter extends JavaTokenParsers{
    type E = Expr
    
    //  CompoundAssignOp   (left: Expr, right: Expr, operator: String) 
    def assignment: Parser[Expr] = (ident ~ "(\\+|-|^|&|<<|>>|\\*|\\||)=".r) ~ expr ^^ {  case x ~ op ~ y => CompoundAssignOp(DeclRefExpr("",x,"",""),y,op) }
        
    def literal : Parser[Expr]= funCall |
        ident               ^^ { case id => DeclRefExpr("",id,"","") } | 
        floatingPointNumber ^^ { case x => 
            if (x.toInt == x.toDouble) Literal("double", x) else Literal("int", x)
        }
    def unary: Parser[String] =  "\\+\\+|--|-(?=[^=])|~(?=[^=])|&(?=[^=])|\\*(?=[^=])|!(?=[^=])".r
    def bop0: Parser[String]  = "\\*(?=[^=])|/(?=[^=])|%".r
    def bop1: Parser[String]  = "\\+(?=[^=])|-(?=[^=])".r
    def bop2: Parser[String]  = "<<(?=[^=])|>>(?=[^=])".r
    def bop3: Parser[String]  = "<|<=|>|>=".r
    def bop4: Parser[String]  = "==|!=".r
    def bop5: Parser[String]  = "&(?=[^&=])".r
    def bop6: Parser[String]  = "\\^".r
    def bop7: Parser[String]  = "\\|(?=[^=\\|])".r
    def bop8: Parser[String]  = "&&".r
    def bop9: Parser[String]  = "\\|\\|".r

    
    def recurrenceRelation(expr:Parser[E], parsop:Parser[E=>E] )    = expr ~ rep(parsop) ^^ { case a ~ b   => (a /: b)((acc,f) => f(acc)) }
    def parseOp(bop: Parser[String], expr: Parser[E]): Parser[E=>E] = bop ~ expr ^^ { case op ~ y => BinaryOp(_,y,op) }
     
    def expr: Parser[Expr]  =  assignment | expr10
    def paren = "(" ~> expr <~ ")"
    def expr0: Parser[Expr] = paren | 
     ((unary?) ~ (paren | literal) ~ (unary?)) ^^ {
         case None    ~ el ~ Some(u) if (u == "++" || u == "--") => UnaryOp(el,u,OpPosition("postfix"))
         case Some(u) ~ el ~ None     => UnaryOp(el,u,OpPosition("prefix" ))
         case None    ~ el ~ None     => el
     }
     
      def expr1: Parser[E]  = recurrenceRelation(expr0, parseOp(bop0,expr0))
      def expr2: Parser[E]  = recurrenceRelation(expr1, parseOp(bop1,expr1))
      def expr3: Parser[E]  = recurrenceRelation(expr2, parseOp(bop2,expr2))
      def expr4: Parser[E]  = recurrenceRelation(expr3, parseOp(bop3,expr3))
      def expr5: Parser[E]  = recurrenceRelation(expr4, parseOp(bop4,expr4))
      def expr6: Parser[E]  = recurrenceRelation(expr5, parseOp(bop5,expr5))
      def expr7: Parser[E]  = recurrenceRelation(expr6, parseOp(bop6,expr6))
      def expr8: Parser[E]  = recurrenceRelation(expr7, parseOp(bop7,expr7))
      def expr9: Parser[E]  = recurrenceRelation(expr8, parseOp(bop8,expr8))
      def expr10: Parser[E] = recurrenceRelation(expr9, parseOp(bop9,expr9))
      
     def params: Parser[List[Expr]] = ((rep(expr <~ ",") ~ expr)?) ^^ { 
         case None => Nil
         case Some(list ~ expr) => list :+ expr
     }
     // params.head is the name of the function
     def funCall: Parser[Expr]    = (ident <~ "(") ~ params <~ ")" ^^ { case id ~ params => 
         CallExpr("",DeclRefExpr("",id,"","") :: params)
     }
     
     def main(args : Array[String]) = {
         /*println(Array("\\+\\+|--|-|~|&|\\*|!(?=[^=])","\\*|/|%","\\+|-","<<|>>","<|<=|>|>=","==|!=","&","^","&&","\\|\\|")
             .find("".matches(_)))*/
         var numTest = 0
         var errors = 0
         var filename = "ModelChecker/unitary_tests/Parser/test.txt"
         Source.fromFile(filename).getLines.takeWhile(_ != ".end.").foreach(line => 
          try {
              numTest += 1
              println("_________Test line nb: %d_________".format(numTest))
              println(line)
              val res = parseAll(expr ,line).get
              println(res /*match {
                  case BinaryOp(_,_,op) => op + 2
                  case _ => res
              }*/)
             
          } catch {
              case e: Throwable => println("Test failed on line %d" .format(numTest)) ;errors += 1; e.printStackTrace
          })
      if (errors == 0) println("All tests ran successfully :) !") 
     }
}