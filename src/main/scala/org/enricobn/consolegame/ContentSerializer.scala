package org.enricobn.consolegame

import org.enricobn.vfs.IOError

/**
  * Created by enrico on 1/2/17.
  */
trait ContentSerializer[T <: AnyRef] {

  def toString(content: T) : Either[IOError, String]

  // TODO Either[IoError, T]
  def fromString(s: String) : Either[IOError, T]

  val clazz: Class[T]

}
