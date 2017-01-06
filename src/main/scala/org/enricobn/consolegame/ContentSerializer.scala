package org.enricobn.consolegame

import org.enricobn.vfs.IOError
import IOError._
import upickle.default._

/**
  * Created by enrico on 1/2/17.
  */
trait ContentSerializer[T <: AnyRef] {

  def toString(content: T) : Either[IOError, String]

  def fromString(s: String) : Either[IOError, T]

  val clazz: Class[T]

  protected def writeE[T1: Writer](value: T1) : Either[IOError, String] = {
    try {
      Right(write(value))
    } catch {
      case e: Throwable => e.getMessage.ioErrorE
    }
  }

  protected def readE[T1: Reader](s: String) : Either[IOError, T1] = {
    try {
      Right(read[T1](s))
    } catch {
      case e: Throwable => e.getMessage.ioErrorE
    }

  }
}
