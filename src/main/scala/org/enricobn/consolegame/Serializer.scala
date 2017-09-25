package org.enricobn.consolegame

import org.enricobn.vfs.IOError

trait Serializer {

  val name: String

  val clazz: Class[_]

  def serialize(content: AnyRef) : Either[IOError,String]

  def deserialize(ser: String) : Either[IOError,AnyRef]

}
