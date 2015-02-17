package ctl.experiment.generics

/**
 * @author Zohour Abouakil
 */
abstract class Environment[T] {
    protected type Env = Environment[T]
    
    def unary_! : Set[Env]
    def interEnv (that: Env): Env 
    def -(name: String): Env
}

case class Bottom[T]() extends Environment[T] {
    override def unary_!             = Set(Bindings[T]())
    override def interEnv(that: Env) = new Bottom[T]
    override def -(name: String)     = Bottom[T]()
    override def toString            = "Bottom"
    override def equals(a: Any)      = a.isInstanceOf[Bottom[T]]
}

case class Bindings[T](val pos: Map[String, T]=Map[String, T](), val neg: Map[String, Set[T]]=Map[String, Set[T]]()) extends Environment[T] {
   override def toString() = "(Env : {"+pos+"}{"+neg+"}"
   
   override def unary_! = {
       val negative = pos.map     { case (key,value) => new Bindings(Map(),Map(key -> Set(value)))      }
       val positive = neg.flatMap { case (key,set)   => set.map(v => new Bindings(Map(key -> v),Map())) }.toSet
       positive ++ negative
   }
   
   /**
    * This function verifies that the two Env of which we want to compute the intersection are not in conflict 
    */
   def conflicts (that: Env): Boolean = that match {
       case Bottom() => true
       case Bindings(pos,neg) =>
           val domThis = this.pos.keySet ++ this.neg.keySet
           val domThat = pos.keySet ++ neg.keySet
           
           val dom     = domThis & domThat
           
           for(key <- dom){ 
               (this.pos.get(key), pos.get(key),
                       this.neg.get(key), neg.get(key)) match {
                   
                   case (Some(pos1), Some(pos2), None      , None)       => if(pos1 == pos2)        return true
                   case (None      , Some(pos2), Some(neg1), None)       => if(neg1.contains(pos2)) return true 
                   case (Some(pos1), None      , None      , Some(neg2)) => if(neg2.contains(pos1)) return true
                   case  _                                               => 
               }
           }
           false 
   }
   
   override def interEnv (that: Env): Env = {
       that match {
           case Bottom()          => Bottom[T]()  
           case Bindings(pos,neg) => 
               if (conflicts(that)) Bottom[T]()
               else {
                   val resPos = this.pos ++ pos  
                   val resNeg = Map(
                      (this.neg.keySet ++ neg.keySet)
                            .filterNot(resPos contains _)
                            .map(k => k -> (this.neg.getOrElse(k,Set()) ++ neg.getOrElse(k,Set())))
                            .toSeq : _*)
                   Bindings[T](resPos,resNeg)
               }
       }
   } 
    
   def -(name : String): Env = new Bindings(this.pos - name,this.neg - name)
   
   override def equals(a: Any) = a match {
       case Bindings(pos,neg) => pos == this.pos && neg == this.neg
       case _                 => false
   }
}

object Neg {
    def apply[T](neg: (String,Set[T])*) = Bindings(Map(),Map(neg: _*))
}

object Pos {
    def apply[T](pos: (String,T)*) = Bindings(Map(pos: _*),Map())
}
