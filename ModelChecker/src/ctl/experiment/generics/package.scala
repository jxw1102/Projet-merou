package ctl.experiment

package object generics {
    implicit def toNoType[T](name : String) = NoType(name)
}