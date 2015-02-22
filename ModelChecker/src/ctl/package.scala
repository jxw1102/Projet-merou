import scala.reflect.runtime.universe._

package object ctl {
	def AF[M <: MetaVariable: TypeTag, N: TypeTag, V <: Value: TypeTag](p: CtlExpr[M,N,V]): CtlExpr[M,N,V] = AU(True,p)
	def EF[M <: MetaVariable: TypeTag, N: TypeTag, V <: Value: TypeTag](p: CtlExpr[M,N,V]): CtlExpr[M,N,V] = EU(True,p)
	def AG[M <: MetaVariable: TypeTag, N: TypeTag, V <: Value: TypeTag](p: CtlExpr[M,N,V]): CtlExpr[M,N,V] = Not(EF(Not(p)))
	def EG[M <: MetaVariable: TypeTag, N: TypeTag, V <: Value: TypeTag](p: CtlExpr[M,N,V]): CtlExpr[M,N,V] = Not(AF(Not(p)))
	    
	implicit def trueTotrue  [M <: MetaVariable: TypeTag, N: TypeTag, V <: Value: TypeTag](t: True) : Predicate[M,N,V] = 
        Predicate(Labelizer.createTrue[M,N,V])
        
    implicit def falseTofalse[M <: MetaVariable: TypeTag, N: TypeTag, V <: Value: TypeTag](t: False): Predicate[M,N,V] = 
        Predicate(Labelizer.createFalse[M,N,V])
}