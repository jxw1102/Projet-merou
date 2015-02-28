package ctl.test

import scala.reflect.runtime.universe
import ctl.AG
import ctl.Bottom
import ctl.BottomEnv
import ctl.ConvertEnv
import ctl.MetaVariable
import ctl.True
import ctl.Value
import ctl.CtlExpr

object Main extends App with ConvertEnv {
  class MyVariable extends MetaVariable
  class MyValue    extends Value
  
  class PatateVariable extends MetaVariable
  class PatateValue    extends Value
  
  val b1 = BottomEnv.create[MyVariable, MyValue]
  val b2 = Bottom & b1
  
  val b3 = BottomEnv.create[PatateVariable, PatateValue]
  val b4 = Bottom & b3
  val b5: BottomEnv[PatateVariable, PatateValue] = Bottom
  
  val b6 = Bottom
  
  val t1: CtlExpr[MyVariable,Any,MyValue]    = True
  val t2: CtlExpr[MyVariable,String,MyValue] = True
  
  println(b1 == b2)
  println(b1 == b3)
  println(b1 == b4)
  
  println(b2 == b3)
  println(b2 == b4)
  
  println(b3 == b4)
  println(b3 == b5)
  
  println(t1 == t2)
}