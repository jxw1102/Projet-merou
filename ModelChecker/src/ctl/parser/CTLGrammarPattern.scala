package ctl.parser

import scala.util.parsing.combinator._
import ast.ProgramNode
import ast.model.BinaryOp
import ast.model.CallExpr
import ast.model.DeclRefExpr
import ast.model.Expr
import ast.model.CompoundAssignOp
import ast.model._
import ast.model.Literal
import ast.model.OpPosition
import ast.model.UnaryOp
import ctl._
import ctl.CtlExpr
import cfg._
import scala.io.Source

class CTLGrammarPattern extends JavaTokenParsers  {
   type M = CFGMetaVar
   type N =  ProgramNode
   type V = CFGVal
   type E = Expr
   type EP = ExprPattern
   type L = Labelizer[M,N,V]
   
//    def concat(list:List[CtlExpr[M,N,V]], op:(CtlExpr[M,N,V],CtlExpr[M,N,V]) => CtlExpr[M,N,V]):CtlExpr[M,N,V]  = list match {
//     case List(elem) => elem
//     case elem::tail => elem || concatOR(tail)
//     }
    def concatOR(list:List[CtlExpr[M,N,V]]):CtlExpr[M,N,V]  = list match {
     case List(elem) => elem
     case elem::tail => elem || concatOR(tail)
     }
    def concatAND(list:List[CtlExpr[M,N,V]]):CtlExpr[M,N,V]  = list match {
     case List(elem) => elem
     case elem::tail => elem && concatAND(tail)
     }
      
  lazy val ctl5: Parser[CtlExpr[M,N,V]] = ctl4 ~ rep(("OR") ~> ctl4) ^^ { 
    case left ~ List() => left
    case left ~ list => left || concatOR(list)
//    case left ~ listRight =>  left || concatOR(listRight(0), listRight.tail)  // faire pour tous les elements
    } 
  lazy val ctl4: Parser[CtlExpr[M,N,V]] = ctl3 ~ rep(("AND") ~> ctl3) ^^ { 
    case left ~ List() => left
    case left ~ list => left &&  concatAND(list)
    } 
  lazy val  ctl3:Parser[CtlExpr[M,N,V]] = "A" ~> "[" ~> ctl3 ~ ("U" ~> ctl3 <~ "]")                ^^  { case x ~ y => AU(x,y)}    |
                                                   "E" ~> "[" ~> ctl3 ~ ("U" ~> ctl3 <~ "]")                 ^^ { case x ~ y => EU(x,y) }           |
                                                   "AX"  ~> "(" ~> ctl3 <~ ")"                                           ^^  { case x => AX(x) }                              |
                                                   "EX" ~> ctl3                                                                     ^^  { case x => EX(x)}                               |
                                                   ((("exists" ~>  ident) <~ "(") ~ ctl5 <~ ")") ^^ { case x ~ y => Exists( (CFGMetaVar(x),new NoType[V]) , y) }                           | 
                                                   "NOT" ~> "(" ~> ctl5 <~ ")" ^^ { case x => !x }                                                                             | ctl2
//  lazy val ctl2:Parser[CtlExpr[M,N,V]] = ctl1 | exp14 ^^ { case x => Predicate(ExpressionLabelizer(x)) } /* DefinedExpr(x)*/
  lazy val ctl2:Parser[CtlExpr[M,N,V]] = ctl1 | labelizer ^^ { case x => Predicate(x) } /* DefinedExpr(x)*/
  lazy val ctl1:Parser[CtlExpr[M,N,V]] = "(" ~> ctl5 <~ ")"
  
  
  /***********************************************************************************/
  
  val labelizer:Parser[Labelizer[M,N,V]] =  "if" ~> "(" ~> exp14 <~ ")" ^^ { case exp => IfLabelizer(exp)} |
                                                              "while" ~> "(" ~> exp14 <~ ")" ^^ { case exp => WhileLabelizer(exp)} |
                                                              "for" ~> "(" ~>  opt(exp14) <~ ")" ^^ { case None => ForLabelizer(None)
                                                                                                                                      case Some(exp) => ForLabelizer(Some(exp))} |
                                                              "switch" ~> "(" ~> exp14 <~ ")" ^^ { case exp => SwitchLabelizer(exp)} |
                                                               "def" ~> "(" ~> expIdent <~ ")" ^^ {case x => VarDefLabelizer(VarDefPattern(DefinedString(""), DefinedString(x))) } |
                                                               "decl" ~> "(" ~> expIdent <~ ")" ^^ {case x => VarDeclLabelizer(VarDeclPattern(DefinedString(""), DefinedString(x))) } |
                                                              exp14 ^^ { case x => ExpressionLabelizer(x)} 
  
                                                               
  /***********************************************************************************/
                                                               
  def bopRec(list:List[~[String,EP]]):EP = list match {
     case List(~(op,elem)) => elem
     case ~(op,elem)::tail =>  BinaryOpPattern(elem,bopRec(tail),DefinedString(tail(0)._1))
     }
  
  val exp14 :Parser[EP] = ("ass" ~> "(" ~> expIdent) ~ ("="|"+="|"-="|"*="|"/="|"%="|"&="|"|="|"^="|"<<="|">>=") ~ exp13 <~ ")" ^^ {  
    case ident ~ op ~ exp => AssignmentPattern(DefinedExpr(DeclRefExpr("",ident,"")), exp, DefinedString(op)) 
    } |
    opt(expIdent ~ ("="|"+="|"-="|"*="|"/="|"%="|"&="|"|="|"^="|"<<="|">>=")) ~ exp13 ^^ { 
    case None ~ right => right
    case Some(ident ~ "=") ~ right => BinaryOpPattern(DefinedExpr(DeclRefExpr("",ident,"")),right,DefinedString("="))
    case Some(ident ~ op) ~ right => CompoundAssignOpPattern(DefinedExpr(DeclRefExpr("",ident,"")),right,DefinedString(op))
  } | exp13 ^^ { case x => x }
  
  lazy val exp13:Parser[EP] = exp12
//  "if" ~> "(" ~> exp12 <~ ")" ^^ { case exp => "if:("+ exp +")"} |
//                                                              "while" ~> "(" ~> exp12 <~ ")" ^^ { case exp => "while:("+ exp +")"} |
//                                                              "for" ~> "(" ~> exp12 <~ ")" ^^ { case exp => "for:("+ exp +")"} |
//                                                              "switch" ~> "(" ~> exp12 <~ ")" ^^ { case exp => "switch:("+ exp +")"} |
//                                                              exp12 ^^ { case x => x }
  
  lazy val exp12: Parser[EP] = exp11 ~ rep("||" ~ exp11) ^^ { 
    case left ~ List() => left
    case left ~ right => BinaryOpPattern(left,bopRec(right),DefinedString(right(0)._1))  //  "exp12:("+left +","+ right+")"
   }
  lazy val exp11: Parser[EP] = exp10 ~ rep("&&" ~ exp10) ^^ { 
    case left ~ List() => left
    case left ~ right => BinaryOpPattern(left,bopRec(right),DefinedString(right(0)._1))
    } 
  lazy val exp10: Parser[EP] = exp9 ~ rep("|" ~ exp9) ^^ { 
    case left ~ List() => left
    case left ~ right => BinaryOpPattern(left,bopRec(right),DefinedString(right(0)._1))
    } 
  lazy val exp9: Parser[EP] = exp8 ~ rep("^" ~ exp8) ^^ { 
    case left ~ List() => left
    case left ~ right => BinaryOpPattern(left,bopRec(right),DefinedString(right(0)._1))
    } 
  lazy val exp8: Parser[EP] = exp7 ~ rep("&" ~ exp7) ^^ { 
    case left ~ List() => left
    case left ~ right => BinaryOpPattern(left,bopRec(right),DefinedString(right(0)._1))
    } 
  lazy val exp7: Parser[EP] = exp6 ~ opt(("=="|"!=") ~ exp6) ^^ { 
    case left ~ None => left
    case left ~ Some(op ~ rightExp) => BinaryOpPattern(left,rightExp,DefinedString(op))
}
  lazy val exp6: Parser[EP] = exp5 ~ opt(("<="|">="|"<"|">") ~ exp5) ^^ { 
    case left ~ None => left
    case left ~ Some(op ~ rightExp) => BinaryOpPattern(left,rightExp,DefinedString(op))
}
  lazy val exp5: Parser[EP] = exp4 ~ opt(("<<"|">>") ~ exp4) ^^ { 
    case left ~ None => left
    case left ~ Some(op ~ rightExp) => BinaryOpPattern(left,rightExp,DefinedString(op))
    }
  lazy val exp4: Parser[EP] = exp3 ~ rep(("+"|"-") ~ exp3) ^^ { 
    case left ~ List() => left
    case left ~ right => BinaryOpPattern(left,bopRec(right),DefinedString(right(0)._1))
    } 
  lazy val exp3: Parser[EP] = exp3bis ~ rep(("*"|"/"|"%") ~ exp3bis)^^ { 
    case left ~ List() => left
    case left ~ right => BinaryOpPattern(left,bopRec(right),DefinedString(right(0)._1))
    } 
   lazy val exp3bis: Parser[EP] = exp3bisbis ~ rep(("op" ~> "(" ~> undefinedVar <~ ")") ~ exp3bisbis) ^^ {  //str(AB) : metavar only..
    case left ~ List() => left
    case left ~  list => BinaryOpPattern(left,bopRec(list),UndefinedVar(CFGMetaVar(list(0)._1))) //UndefinedVar(CFGMetaVar(x)) changer rec
    } 
   
  def bopRecNotString(list:List[~[~[String,List[String]],EP]]):EP = list match {
     case List(expNotin ~ listExpNotin ~ exp ) => exp
     case (~( _ , exp) )::list => BinaryOpPattern(exp, bopRecNotString(list), NotString((((list(0)_1)_1)::((list(0)_1)_2)).toSet))
     }
  
   lazy val exp3bisbis: Parser[EP] = exp2 ~ rep(("notin" ~> "(" ~> "" ~> expNotin ~ rep("," ~> expNotin) <~ ")") ~ exp2)^^ {
    case left ~ List() => left
    case left ~ list => BinaryOpPattern(left, bopRecNotString(list), NotString((((list(0)_1)_1)::((list(0)_1)_2)).toSet) )
    } 
   
//  lazy val str = ("str" <~ "(") ~ undefinedVar <~ ")"
//  lazy val notin = "notin" ~ "(" ~ "" ~ expNotin ~ rep("," ~ expNotin) ~ ")"
  lazy val expNotin = ("="|"+"|"-"|"*"|"/"|"%")
                
  lazy val exp2: Parser[EP] = exp1 ~ ("++"|"--")                                                          ^^ { case x ~ op => UnaryOpPattern(x, DefinedString(op)) } |
                                                            opt("!"|"~"|"--"|"++"|"*"|"&"|"-"|"+") ~ exp1 ^^ { case None ~ right => right
                                                                                                                                                                case Some(op) ~ exp => UnaryOpPattern(exp, DefinedString(op)) }                                      
  lazy val exp1: Parser[EP] =  undefinedVar ^^ { case x => UndefinedVar(CFGMetaVar(x))} |
                    "literalExp" ~> "(" ~> undefinedVar <~ ")" ^^ {case x => LiteralExprPattern(UndefinedVar(CFGMetaVar(x))) } |
                    "pointerExp" ~> "(" ~> undefinedVar <~ ")" ^^ {case x => PointerExperPattern(UndefinedVar(CFGMetaVar(x))) } |
//                    "def" ~> "(" ~> expIdent <~ ")" ^^ {case x => VarDefPattern(DefinedString(""), DefinedString(x)) } | //return un DeclPrattern : problem
//                    "decl" ~> "(" ~> expIdent <~ ")" ^^ {case x => VarDeclPattern(DefinedString(""), DefinedString(x)) } |
                    funcall |
                    parenth |
                    floatingPointNumber ^^ { case x => if  (x.toInt == x.toDouble) DefinedExpr(Literal("double", x)) else DefinedExpr(Literal("int", x)) } |
                    expIdent   ^^ { case id => DefinedExpr(DeclRefExpr("",id,""))} 
 lazy val undefinedVar = "[A-Z]+".r
// lazy val pat2: Parser[EP] =            
 lazy val pat1: Parser[EP] = "[A-Z]+".r ^^ { case x => UndefinedVar(CFGMetaVar(x))}
//  lazy val expToPat:Parser[EP] = exp13 ^^ { case x => DefinedExpr(x)}
                    
  lazy val expIdent = "[a-z]\\w*".r
  
  
  
  
  lazy val funcall: Parser[EP] =   
    "call" ~> "(" ~> expIdent <~ ")" ^^ {case ident => CallExprPattern(DefinedString(ident), None) } | //ici il peut y avoir plusieur argument ou aucun
    (expIdent <~ "(") ~ opt(exp12 ~ rep(","~>exp12)) <~ ")" ^^ { 
    case  ident ~ None=> CallExprPattern(DefinedString(ident), Some(Nil))
    case ident ~ Some(exp ~ list) => CallExprPattern(DefinedString(ident), Some(exp::list))}
  
  lazy val parenth: Parser[EP] = "(" ~> exp12 <~ ")"  ^^ { case x => x}  

}

 object MyParseree extends CTLGrammarPattern{
  def calculate(expression: String) = parseAll(ctl5, expression)
  
  def main(args : Array[String]) = {
         var numTest = 0
         var errors = 0
         var filename = "ModelChecker/unitary_tests/Parser/testCTLExpr.txt"
//         var filename = "ModelChecker/unitary_tests/Parser/testExpr.txt"
         Source.fromFile(filename).getLines.takeWhile(_ != ".end.").foreach(line => 
          try {
              numTest += 1
              println("----------------  Test line nb: %d  -----------------".format(numTest))
              println(line)
              val res = calculate(line).get
              println(res /*match {
                  case BinaryOp(_,_,op) => op + 2
                  case _ => res
              }*/)
             println()
          } catch {
              case e => println("Test failed on line %d" .format(numTest)) ;errors += 1; e.printStackTrace
          })
      if (errors == 0) println("All tests ran successfully :) !") 
  }
    
  }