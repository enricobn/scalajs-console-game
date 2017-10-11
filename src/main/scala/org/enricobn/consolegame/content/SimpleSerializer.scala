package org.enricobn.consolegame.content

import org.enricobn.consolegame.Serializer
import org.enricobn.vfs.IOError

abstract class SimpleSerializer[T](val clazz: Class[T]) extends Serializer {

  override def serialize(content: AnyRef): Either[IOError, String] =
    if (content.getClass == clazz) {
      serializeIt(content.asInstanceOf[T])
    } else {
      Left(IOError("Not an instance of " + clazz))
    }

  def serializeIt(content: T): Either[IOError, String]
}
