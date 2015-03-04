//package ctl.parser
//
//import scala.util.parsing.combinator._
//import scala.io.Source
//
//class CTLGrammar_String extends JavaTokenParsers  {
//  
//  lazy val ctl5: Parser[String] = ctl4 ~ rep(("OR") ~ ctl4) ^^ { 
//    case left ~ List() => left
//    case left ~ right => "clt5:("+left+","+right+")"
//    } 
//  lazy val ctl4: Parser[String] = ctl3 ~ rep(("AND") ~ ctl3) ^^ { 
//    case left ~ List() => left
//    case left ~ right => "clt4:("+left+","+right+")"
//    } 
//  lazy val  ctl3:Parser[String] = "A" ~> "[" ~> ctl3 ~ ("U" ~> ctl3 <~ "]")                ^^  { case x ~ y => "ctl3:("+x +" AU "+ y +")"}    |
//                                                   "E" ~> "[" ~> ctl3 ~ ("U" ~> ctl3 <~ "]")                 ^^ { case x ~ y => "ctl3:("+x +" EU "+ y +")" }           |
//                                                   "AX"  ~> "(" ~> ctl3 <~ ")"                                           ^^  { case x => "ctl3:AX("+x+")" }                              |
//                                                   "EX" ~> ctl3                                                                     ^^  { case x => "ctl3:EX("+x+")"}                               |
//                                                   ((("exists" ~>  ident) <~ "(") ~ ctl5 <~ ")") ^^ { case x ~ y => "ctl3:EXISTS "+x+"("+y+")" }                   | 
//                                                   "NOT" ~> "(" ~> ctl5 <~ ")" ^^ { case x => "ctl3:NOT("+x+")" }                                                                     | ctl2
//  lazy val ctl2:Parser[String] = assignment  | ctl1 
//  lazy val ctl1:Parser[String] = "(" ~> ctl5 <~ ")"    ^^ { case x => "parenth:("+x+")"}  
//  
//  
//  /***********************************************************************************/
//  
//  val assignment :Parser[String] = //ident ^^  { case ident => "ident : "+ident }
//  opt(ident ~ ("="|"+="|"-="|"*="|"/="|"%="|"&="|"|="|"^="|"<<="|">>=")) ~ exp13 ^^ { 
//    case None ~ right => right
//    case Some(leftIdent ~ leftOp) ~ right => "ass:("+leftIdent+ " "+leftOp + " "+ right +")"
//  } | exp13 ^^ { case x => x } 
//  
//  lazy val exp13:Parser[String] = "if" ~> "(" ~> exp12 <~ ")" ^^ { case exp => "if:("+ exp +")"} |
//                                                              "while" ~> "(" ~> exp12 <~ ")" ^^ { case exp => "while:("+ exp +")"} |
//                                                              "for" ~> "(" ~> exp12 <~ ")" ^^ { case exp => "for:("+ exp +")"} |
//                                                              "switch" ~> "(" ~> exp12 <~ ")" ^^ { case exp => "switch:("+ exp +")"} |
//                                                              exp12 ^^ { case x => x }
//                                                              
//  lazy val exp12: Parser[String] = exp11 ~ rep("||" ~ exp11) ^^ { 
//    case left ~ List() => left
//    case left ~ right => "exp12:("+left +","+ right+")"
//   }
//  lazy val exp11: Parser[String] = exp10 ~ rep("&&" ~ exp10) ^^ { 
//    case left ~ List() => left
//    case left ~ right => "exp11:("+left+","+right+")"
//    } 
//  lazy val exp10: Parser[String] = exp9 ~ rep("|" ~ exp9) ^^ { 
//    case left ~ List() => left
//    case left ~ right => "exp10:("+left+","+right+")"
//    } 
//  lazy val exp9: Parser[String] = exp8 ~ rep("^" ~ exp8) ^^ { 
//    case left ~ List() => left
//    case left ~ right => "exp9:("+left+","+right+")"
//    } 
//  lazy val exp8: Parser[String] = exp7 ~ rep("&" ~ exp7) ^^ { 
//    case left ~ List() => left
//    case left ~ right => "exp8:("+left+","+right+")"
//    } 
//  lazy val exp7: Parser[String] = exp6 ~ opt(("=="|"!=") ~ exp6) ^^ { 
//    case left ~ None => left
//    case left ~ Some(rightOp ~ rightExp) => "exp7:("+left+" "+ rightOp + " "+rightExp +")"
//}
//  lazy val exp6: Parser[String] = exp5 ~ opt(("<="|">="|"<"|">") ~ exp5) ^^ { 
//    case left ~ None => left
//    case left ~ Some(rightOp ~ rightExp) => "exp6:("+left+" "+ rightOp + " "+rightExp +")"
//}
//  lazy val exp5: Parser[String] = exp4 ~ opt(("<<"|">>") ~ exp4) ^^ { 
//    case left ~ None => left
//    case left ~ Some(rightOp ~ rightExp) => "exp5:("+left+" "+ rightOp + " "+rightExp +")"
//    }
//  
//  lazy val exp4: Parser[String] = exp3 ~ rep(("+"|"-") ~ exp3) ^^ { 
//    case left ~ List() => left
//    case left ~ right => "exp4:("+left+","+right+")"
//    } 
//  lazy val exp3: Parser[String] = exp2 ~ rep(("*"|"/"|"%") ~ exp2)^^ { 
//    case left ~ List() => left
//    case left ~ right => "exp3:("+left+","+right+")"
//    } 
//  lazy val exp2: Parser[String] = exp1 ~ ("++"|"--")                                                          ^^ { case x ~ op => "exp2:("+x+op+")" } |
//                                                            opt("!"|"~"|"--"|"++"|"*"|"&"|"-"|"+") ~ exp1 ^^ { case None ~ right => right
//                                                                                                                                                                case Some(leftOp) ~ right => "exp2:("+leftOp + right +")" }                                      
//  lazy val exp1: Parser[String] =  rep("[A-Z]".r) ^^ { case x => "METAVAR"}| funcall |parenth |floatingPointNumber | ident
//  lazy val funcall: Parser[String] =    (ident <~ "(") ~ exp12 ~ rep(","~>exp12) <~ ")" ^^ { case  ident ~ exp12 ~ list => "funcall:("+ident+"("+(exp12::list)+")"+")"}
//  lazy val parenth: Parser[String] = "(" ~> exp13 <~ ")"  ^^ { case x => "parenth:("+x+")"}  
//}
//
// object MyParseree extends CTLGrammar_String{
//   
////  def main(args: Array[String]) {
////    println("DEMARRAGE" )
////    println("result: " + calculate("exists k(*5 * ((*f(x)+ 4) - 1) && 3|| *13 + f(x) + 4) "))
//////    println("result: " + calculate("x = 5 * (3 + 4) - 1"))
//////    println("result: " + calculate("x = 5 + 4 - 1"))
//////    println("result: " + calculate("x = 5 + 4 + 1 *4 *3"))
//////    println("result: " + calculate("x = 5 + 4 + *1 * *4 *3"))
////    
//////    println("result: " + calculate("exists x(AX(A [exists expr(2) U AX(expr)])) "))
////    println("result: " + calculate("f(b) OR exists x(g(x) AND AX(f(x))) "))
////    
////    
////    // Parse an expression and return the calculated result as a String
////  }
//  def calculate(expression: String) = parseAll(ctl5, expression)
//  
//  def main(args : Array[String]) = {
//         var numTest = 0
//         var errors = 0
//         var filename = "ModelChecker/unitary_tests/Parser/testCTLExpr.txt"
////         var filename = "ModelChecker/unitary_tests/Parser/testExpr.txt"
//         Source.fromFile(filename).getLines.takeWhile(_ != ".end.").foreach(line => 
//          try {
//              numTest += 1
//              println("----------------  Test line nb: %d  -----------------".format(numTest))
//              println(line)
//              val res = calculate(line).get
//              println(res /*match {
//                  case BinaryOp(_,_,op) => op + 2
//                  case _ => res
//              }*/)
//             println()
//          } catch {
//              case e => println("Test failed on line %d" .format(numTest)) ;errors += 1; e.printStackTrace
//          })
//      if (errors == 0) println("All tests ran successfully :) !") 
//  }
//    
//  }