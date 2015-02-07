package ctl

import ast.model.Expr
import scala.collection.mutable.Map

/**
 * @author simple
 */


abstract class Environment {
    def unapply : Option[(Map[String, Expr],Map[String, Set[Expr]])]
    def unary_! : Set[Environment]
    def interEnv (that: Environment): Environment 
    def -(name: String): Environment
}

object Bottom extends Environment {
	override def unapply                     = None
    override def unary_!                     = Set(new Bindings)
    override def interEnv(that: Environment) = Bottom
    override def -(name: String)             = Bottom
}

class Bindings extends Environment {
   val positiveBindings = Map[String, Expr]()
   val negativeBindings = Map[String, Set[Expr]]() 
   
   def this (pb: Map[String, Expr]=Map(), nb: Map[String, Set[Expr]]=Map()) {
       this()
       positiveBindings ++= pb
       negativeBindings ++= nb
   }
   
   override def unapply: Option[(Map[String, Expr],Map[String, Set[Expr]])] = Some(positiveBindings, negativeBindings)

   override def unary_! = {
       val neg = positiveBindings.map     { case (key,value) => new Bindings(Map(),Map(key -> Set(value)))      }
       val pos = negativeBindings.flatMap { case (key,set)   => set.map(v => new Bindings(Map(key -> v),Map())) }.toSet
       pos ++ neg
   }
   
   /*
    * This function verify if the two environment we want to intersection are not in conflict 
    */
   def conflicts (that: Bindings): Boolean = {
       val domThis = this.positiveBindings.keySet ++ this.negativeBindings.keySet
       val domThat = that.positiveBindings.keySet ++ that.negativeBindings.keySet
       
       val dom     = domThis & domThat
       
       for(key <- dom){ 
           (this.positiveBindings.get(key), that.positiveBindings.get(key),
                   this.negativeBindings.get(key), that.negativeBindings.get(key)) match {
               
               case (Some(valueThis), Some(valueThat), None           , None)            => if(valueThis == valueThat)        return true
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
                   val res = new Bindings
                   res.positiveBindings ++= this.positiveBindings ++ pos
                   val tmp                = this.negativeBindings ++ neg
       
                   // clean 
                   res.negativeBindings ++= tmp.filterNot { case (key,value) => res.positiveBindings.contains(key) }
                   res
              }
       }
   } 
    
   def -(name : String) : Environment = {
       return new Bindings(if(this.positiveBindings contains name) this.positiveBindings-name else this.positiveBindings,
                                if(this.negativeBindings contains name) this.negativeBindings-name else this.negativeBindings)
   }
  
   /*
    * This function is used to get the opposite of an environment 
    */
}