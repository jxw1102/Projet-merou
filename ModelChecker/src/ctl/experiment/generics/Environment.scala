package ctl.experiment.generics

import ast.model.Expr

/**
 * @author Zohour Abouakil
 */
abstract class EnvironmentTest[T] {
    protected type Env = EnvironmentTest[T]
    
    def unary_! : Set[Env]
    def interEnv (that: Env): Env 
    def -(name: String): Env
}

case class BottomTest[T]() extends EnvironmentTest[T] {
    override def unary_!                     = Set(BindingsTest[T]())
    override def interEnv(that: Env) = new BottomTest[T]
    override def -(name: String)             = BottomTest[T]()
    
    override def toString                    = "BottomTest"
}

case class BindingsTest[T](pos: Map[String, T]=Map[String, T](), neg: Map[String, Set[T]]=Map[String, Set[T]]()) extends EnvironmentTest[T] {
//   override def unapply(b: BindingsTest[T]) = Some(b.positiveBindingsTest,b.negativeBindingsTest)
   override def toString() = "(Env : {"+pos+"}{"+neg+"}"
   
   override def unary_! = {
       val negative = pos.map     { case (key,value) => new BindingsTest(Map(),Map(key -> Set(value)))      }
       val positive = neg.flatMap { case (key,set)   => set.map(v => new BindingsTest(Map(key -> v),Map())) }.toSet
       positive ++ negative
   }
   
   /**
    * This function verifies that the two Env of which we want to compute the intersection are not in conflict 
    */
   def conflicts (that: Env): Boolean = that match {
       case BottomTest() => true
       case BindingsTest(pos,neg) =>
	       val domThis = this.pos.keySet ++ this.neg.keySet
	       val domThat = pos.keySet ++ neg.keySet
	       
	       val dom     = domThis & domThat
	       
	       for(key <- dom){ 
	           (this.pos.get(key), pos.get(key),
	                   this.neg.get(key), neg.get(key)) match {
	               
	               case (Some(valueThis), Some(valueThat), None           , None)            => if(valueThis == valueThat)        return true
	               case (None           , Some(valueThat), Some(valueThis), None)            => if(valueThis.contains(valueThat)) return true 
	               case (Some(valueThis), None           , None           , Some(valueThat)) => if(valueThat.contains(valueThis)) return true
	               case  _                                                                   => 
	           }
	       }
	       false 
   }
   
   
//   override def interEnv (that: Env): Env = ???
   
   override def interEnv (that: Env): Env = {
       that match {
           case BottomTest()          => BottomTest[T]()  
           case BindingsTest(pos,neg) => 
               if (conflicts(that)) BottomTest[T]()
               else {
                   val resPos = this.pos ++ pos  
                   val resNeg = Map(
                      (this.neg.keySet ++ neg.keySet)
                      	  .filterNot(resPos contains _)
                      	  .map(k => k -> (this.neg.getOrElse(k,Set()) ++ neg.getOrElse(k,Set())))
                      	  .toSeq : _*)
                   BindingsTest[T](resPos,resNeg)
               }
       }
   } 
    
   def -(name : String): Env = new BindingsTest(this.pos - name,this.neg - name)
}

