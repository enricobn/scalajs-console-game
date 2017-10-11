package org.enricobn.consolegame

import org.enricobn.vfs.IOError

trait Serializer {

  val clazz: Class[_]

  def name: String = clazz.getName

  def serialize(content: AnyRef) : Either[IOError,String]

  def deserialize(ser: String) : Either[IOError,AnyRef]

}
