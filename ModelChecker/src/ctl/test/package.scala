package ctl

package object test {
    implicit class EnvSugar(pos: Map[String,Any]) {
        def U(neg: Map[String,Set[Any]]) = new Bindings(pos,neg)
    }
}