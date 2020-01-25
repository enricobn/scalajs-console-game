package org.enricobn.consolegame

import org.enricobn.vfs.IOError
import org.enricobn.vfs.IOError._
import upickle.default._

object UpickleUtils {

  def writeE[T1: Writer](value: T1, prettyFormat: Boolean = false): Either[IOError, String] = {
    try {
      Right(
        if (prettyFormat)
          write(value, 2)
        else
          write(value)
      )
    } catch {
      case e: Throwable => e.getMessage.ioErrorE
    }
  }

  def readE[T1: Reader](s: String): Either[IOError, T1] = {
    try {
      Right(read[T1](s))
    } catch {
      case e: Throwable => e.getMessage.ioErrorE
    }

  }

}
