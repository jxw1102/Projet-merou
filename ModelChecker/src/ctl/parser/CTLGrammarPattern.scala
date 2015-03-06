package ctl.parser

import scala.util.parsing.combinator._
import ast.model._
import ctl._
import cfg._
import scala.io.Source


/**
 * RESTE : 
 *      TYPE_OF..
 *      NotString..?
 *      exprimer les proprietes !!
 *      doc : 
 *           decrire syntax
 *           specifier l'ordre lecture / priorite operations (ex avec parentheses..)
 *           exemples simple et complexes
 * 
 */
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
  lazy val ctl = "//.*".r ^^ { case x => "comment"} | ctl5 | "\\s*".r ^^ {case x => "space line"}
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
                                                   ((("exists" ~>  ident) <~ "(") ~ ctl5 <~ ")") ^^ { case x ~ y => Exists( (CFGMetaVar(x),CFGExpr) , y) }                           | //CFGExpr ou new NoType[V]
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
                                                               "<..." ~> exp14 ^^ {case exp => FindExprLabelizer(exp) } |
                                                               "<" ~> exp14 <~ ">" ^^ {case exp => MatchExprLabelizer(exp) } |
                                                               "def" ~> "(" ~> stringIdent  ~ opt(","~>stringIdent) <~ ")" ^^ { case x ~ None      => VarDefLabelizer(VarDefPattern(DefinedString(""), x))
                                                                                                                                                                           case x ~ Some(y) => VarDefLabelizer(VarDefPattern(y, x)) } |
                                                               "decl" ~> "(" ~> stringIdent ~ opt(","~>stringIdent) <~ ")" ^^ {case x ~None        => VarDeclLabelizer(VarDeclPattern(DefinedString(""), x)) 
                                                                                                                                                                           case x ~ Some(y) => VarDeclLabelizer(VarDeclPattern(y, x)) } |
                                                              exp14 ^^ { case x => ExpressionLabelizer(x)} 
  
                                                               
  /***********************************************************************************/
                                                               
                                                                                                                                                                           
 lazy val undefinedVar:Parser[UndefinedVar] = "[A-Z]+".r   ^^ {case x => UndefinedVar(CFGMetaVar(x))}                 
  lazy val stringIdent:Parser[StringPattern] = "[a-z]\\w*".r ^^ { case x => DefinedString(x)}
  lazy val expIdent:Parser[ExprPattern] = "[a-z]\\w*".r  ^^ { case x => DefinedExpr(DeclRefExpr("",x,""))}
  
  
  def bopRec(list:List[~[String,EP]]):EP = list match {
     case List(~(op,elem)) => elem
     case ~(op,elem)::tail =>  BinaryOpPattern(elem,bopRec(tail),DefinedString(tail(0)._1))
     }
  def bopRecUndifVar(list:List[~[UndefinedVar,EP]]):EP = list match {
     case List(~(op,elem)) => elem
     case ~(op,elem)::tail =>  BinaryOpPattern(elem,bopRecUndifVar(tail),tail(0)._1)
     }
  val exp14 :Parser[EP] = ("assign" ~> "(" ~> (expIdent|undefinedVar) <~ ",") ~ exp13 ~ opt("," ~> ("="|"+="|"-="|"*="|"/="|"%="|"&="|"|="|"^="|"<<="|">>=")) <~ ")" ^^ {  
    case ident ~ exp ~ None => AssignmentPattern(ident, exp) 
    case ident ~ exp ~ Some(op) => AssignmentPattern(ident, exp, DefinedString(op)) 
    } |
    opt(expIdent ~ ("="|"+="|"-="|"*="|"/="|"%="|"&="|"|="|"^="|"<<="|">>=")) ~ exp13 ^^ { 
    case None ~ right => right
    case Some(ident ~ "=") ~ right => BinaryOpPattern(ident,right,DefinedString("="))
    case Some(ident ~ op) ~ right => CompoundAssignOpPattern(ident,right,DefinedString(op))
  } | exp13 ^^ { case x => x }
  
  lazy val exp13:Parser[EP] = exp12
  
  lazy val exp12: Parser[EP] = exp11 ~ rep("||" ~ exp11) ^^ { 
    case left ~ List() => left
    case left ~ right => BinaryOpPattern(left,bopRec(right),DefinedString(right(0)._1))  
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
   lazy val exp3bis: Parser[EP] = exp3bisbis ~ rep(("op" ~> "(" ~> undefinedVar <~ ")") ~ exp3bisbis) ^^ {  //op(AB) : metavar only..
    case left ~ List() => left
    case left ~  list => BinaryOpPattern(left,bopRecUndifVar(list),list(0)._1) 
    } 
   
  def bopRecNotString(list:List[~[Set[String],EP]]):EP = list match {
     case List(listExpNotin ~ exp ) => exp
     case (~( _ , exp) )::list => BinaryOpPattern(exp, bopRecNotString(list), NotString(list(0)_1))
     }
  
   lazy val exp3bisbis: Parser[EP] = exp2 ~ rep(notin ~ exp2)^^ {
    case left ~ List() => left
    case left ~ list => BinaryOpPattern(left, bopRecNotString(list), NotString(list(0)_1))
    } 
//  lazy val notin =  ("notin" ~ ("(" ~> "" ~> expNotin) ~ rep("," ~> expNotin) <~ ")") 
  lazy val notin:Parser[Set[String]] =  ("notin" ~> "(" ~> "" ~> expNotin ~ rep("," ~> expNotin) <~ ")") ^^ { case exp ~ list =>  (exp::list).toSet }
  lazy val notinU:Parser[Set[String]] =  ("notinU" ~> "(" ~> "" ~> expNotin ~ rep("," ~> expNotin) <~ ")") ^^ { case exp ~ list =>  (exp::list).toSet }
//  lazy val str = ("str" <~ "(") ~ undefinedVar <~ ")"
//  lazy val notin = "notin" ~ "(" ~ "" ~ expNotin ~ rep("," ~ expNotin) ~ ")"
  lazy val expNotin = ("="|"+"|"-"|"*"|"/"|"%"|"void")
                
  lazy val exp2: Parser[EP] = exp1 ~ ("++"|"--"|notinU)                                                     ^^ { 
  case exp ~ (set:Set[String]) =>  UnaryOpPattern(exp, NotString(set))
  case exp ~ (op:String) => UnaryOpPattern(exp, DefinedString(op))  }|
                                                            opt("!"|"~"|"--"|"++"|"*"|"&"|"-"|"+"|notinU) ~ exp1 ^^ { case None ~ right => right
                                                                                                                                                                case Some(set:Set[String]) ~ exp => UnaryOpPattern(exp, NotString(set))
                                                                                                                                                                case Some(op:String) ~ exp => UnaryOpPattern(exp, DefinedString(op)) } 
  
  lazy val exp1: Parser[EP] =  undefinedVar ^^ { case x => x} |
                    "literalExp" ~> "(" ~> undefinedVar <~ ")" ^^ {case x => LiteralExprPattern(x) } |
                    "pointerExp" ~> "(" ~> undefinedVar <~ ")" ^^ {case x => PointerExperPattern(x) } |
                    funcall |
                    parenth |
                    floatingPointNumber ^^ { case x => if  (x.toInt == x.toDouble) DefinedExpr(Literal("double", x)) else DefinedExpr(Literal("int", x)) } |
                    expIdent   /*^^ { case id => DefinedExpr(DeclRefExpr("",id,""))} */
                 
                  
  
  // typeof ?
  lazy val funcall: Parser[EP] =   
    ("call" ~> "(" ~> (stringIdent|undefinedVar)) ~ opt("," ~> (notin|stringIdent)) <~ ")" ^^ {
      case ident ~ None => CallExprPattern(ident, None) 
    case ident ~ Some(ident2:StringPattern) => CallExprPattern(ident, None,ident2) 
    case ident ~ Some(set:Set[String]) => CallExprPattern(ident, None,NotString(set)) 
    } | //ici il peut y avoir plusieur argument ou aucun
    ((stringIdent|undefinedVar) <~ "(") ~ opt(exp12 ~ rep(","~>exp12)) <~ ")" ^^ { 
    case  ident ~ None=> CallExprPattern(ident, Some(Nil))
    case ident ~ Some(exp ~ list) => CallExprPattern(ident, Some(exp::list))}
  
  lazy val parenth: Parser[EP] = "(" ~> exp12 <~ ")"  ^^ { case x => x}  

}

 object MyParseree extends CTLGrammarPattern{
  def calculate(expression: String) = parseAll(ctl, expression)
  
  def main(args : Array[String]) = {
         var numTest = 0
         var errors = 0
         var filename = "ModelChecker/unitary_tests/Parser/properties.txt"
//         var filename = "ModelChecker/unitary_tests/Parser/testCTLExpr.txt"
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