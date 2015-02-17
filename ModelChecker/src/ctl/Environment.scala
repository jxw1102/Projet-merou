package ctl

import ast.model.Expr

/**
 * @author Zohour Abouakil
 */
abstract class Environment {
    def unapply : Option[(Map[String, Any],Map[String, Set[Any]])]
    def unary_! : Set[Environment]
    def interEnv (that: Environment): Environment 
    def -(name: String): Environment
}

object Bottom extends Environment {
    override def unapply                     = None
    override def unary_!                     = Set(new Bindings)
    override def interEnv(that: Environment) = Bottom
    override def -(name: String)             = Bottom
    
    override def toString                    = "Bottom"
}

class Bindings extends Environment {
   var positiveBindings = Map[String, Any]()
   var negativeBindings = Map[String, Set[Any]]() 
   
   def this (pb: Map[String, Any]=Map(), nb: Map[String, Set[Any]]=Map()) {
       this()
       positiveBindings = pb
       negativeBindings = nb
   }
   
   override def unapply: Option[(Map[String, Any],Map[String, Set[Any]])] = Some(positiveBindings, negativeBindings)
   override def toString() = "(Env : {"+positiveBindings+"}{"+negativeBindings+"}"
   
   override def unary_! = {
       val neg = positiveBindings.map     { case (key,value) => new Bindings(Map(),Map(key -> Set(value)))      }
       val pos = negativeBindings.flatMap { case (key,set)   => set.map(v => new Bindings(Map(key -> v),Map())) }.toSet
       pos ++ neg
   }
   
   /**
    * This function verifies that the two environment of which we want to compute the intersection are not in conflict 
    */
   def conflicts (that: Bindings): Boolean = {
       val domThis = this.positiveBindings.keySet ++ this.negativeBindings.keySet
       val domThat = that.positiveBindings.keySet ++ that.negativeBindings.keySet
       
       val dom     = domThis & domThat
       
       for(key <- dom){ 
           (this.positiveBindings.get(key), that.positiveBindings.get(key),
                   this.negativeBindings.get(key), that.negativeBindings.get(key)) match {
               case (Some(valueThis), Some(valueThat), None           , None)            => if(valueThis != valueThat)        return true
               case (None           , Some(valueThat), Some(valueThis), None)            => if(valueThis.contains(valueThat)) return true 
               case (Some(valueThis), None           , None           , Some(valueThat)) => if(valueThat.contains(valueThis)) return true
               case  _                                                                   => 
           }
       }
       false 
   }
   
   override def interEnv (that: Environment): Environment = {
       that.unapply match {
           case None            => Bottom    
           case Some((pos,neg)) =>
               if (conflicts(that.asInstanceOf[Bindings])) Bottom
               else {
                   val res  = new Bindings
                   val keys = this.negativeBindings.keySet ++ neg.keySet
                    
                   res.positiveBindings = this.positiveBindings ++ pos   
                   res.negativeBindings = 
                       Map(keys
                           .filterNot(res.positiveBindings contains _)
                           .map(k => k -> (this.negativeBindings.getOrElse(k,Set()) ++ neg.getOrElse(k,Set())))
                           .toSeq : _*) 
  
                           
                   res
              }
       }
   } 
    
   def -(name : String): Environment = new Bindings(this.positiveBindings - name,this.negativeBindings - name)
}