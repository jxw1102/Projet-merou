package ctl.experiment.generics.simple.gen

object Main extends App with Convert {
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
  
  println(b1 == b2)
  println(b1 == b3)
  println(b1 == b4)
  
  println(b2 == b3)
  println(b2 == b4)
  
  println(b3 == b4)
  println(b3 == b5)
  
  println(b1 == b6)
}
