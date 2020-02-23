package org.enricobn.buyandsell.content

import org.enricobn.vfs.IOError

import scala.util.Try

object EitherEnumeration {

  implicit class EitherEnumeration[T <: Enumeration](val x: T) extends AnyVal {

    def withNameE(value: String) : Either[IOError, x.Value] =
      Try(x.withName(value)).toOption.toRight(IOError(s"Cannot find enum for '$value'."))

  }

}
