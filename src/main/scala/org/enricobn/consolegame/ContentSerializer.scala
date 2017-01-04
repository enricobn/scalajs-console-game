package org.enricobn.consolegame

/**
  * Created by enrico on 1/2/17.
  */
trait ContentSerializer[T <: AnyRef] {

  // TODO Either[IoError, String]
  def toString(content: T) : String

  // TODO Either[IoError, T]
  def fromString(s: String) : T

  val clazz: Class[T]

}
