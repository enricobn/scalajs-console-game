package org.enricobn.consolegame

/**
  * Created by enrico on 1/2/17.
  */
trait ContentSerializer[T <: AnyRef] {

  def toString(content: T) : String

  def fromString(s: String) : T

  val clazz: Class[T]

}
