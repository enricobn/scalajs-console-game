package org.enricobn.consolegame.content

import org.enricobn.consolegame.UpickleUtils
import org.enricobn.vfs.IOError
import org.enricobn.vfs.impl.Passwd

object PasswdSerializer extends SimpleSerializer(classOf[Passwd]){

  override def serializeIt(content: Passwd): Either[IOError, String] = UpickleUtils.writeE(content)

  override def deserialize(ser: String): Either[IOError, Passwd] = UpickleUtils.readE[Passwd](ser)

}
