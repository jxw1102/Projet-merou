package ctl

import ast.model.Expr
import scala.collection.mutable.Map

/**
 * @author simple
 */

class Environment {
   val positiveBindings = Map[String, Expr]()
   val negativeBindings = Map[String, Set[Expr]]() 
   
   def this (pb: Map[String, Expr], nb: Map[String, Set[Expr]]) {
       this();
       positiveBindings ++= pb
       negativeBindings ++= nb
   }
   
   /*
    * This function verify if the two environment we want to intersection are not in conflict 
    */
   def conflicts (that: Environment): Boolean = {
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
   
   def interEnv (that: Environment): Option[Environment] = {
       if (conflicts(that))
           return None
           
       val res = new Environment
       
       res.positiveBindings ++= this.positiveBindings ++ that.positiveBindings
       val tmp = this.negativeBindings ++ that.negativeBindings
       
       // clean 
       res.negativeBindings ++= tmp.filterNot { case (key,value) => res.positiveBindings.contains(key) }
       
       return Some(res)
   } 
    
   def -(name : String) : Environment = {
       return new Environment(if(this.positiveBindings contains name) this.positiveBindings-name else this.positiveBindings,
                                if(this.negativeBindings contains name) this.negativeBindings-name else this.negativeBindings)
   }
  
}