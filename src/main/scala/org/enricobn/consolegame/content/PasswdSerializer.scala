package org.enricobn.consolegame.content

import org.enricobn.consolegame.UpickleUtils
import org.enricobn.vfs.impl.{AuthenticatedUser, Passwd}
import org.enricobn.vfs.{Authentication, IOError}
import upickle.default.{macroRW, ReadWriter as RW}

object PasswdSerializer extends SimpleSerializer(classOf[Passwd]) {
  implicit val rw: RW[Passwd] = macroRW
  implicit val rwuser: RW[AuthenticatedUser] = macroRW
  implicit val rwauth: RW[Authentication] = macroRW

  override def serializeIt(content: Passwd): Either[IOError, String] = UpickleUtils.writeE(content)

  override def deserialize(ser: String): Either[IOError, Passwd] = UpickleUtils.readE[Passwd](ser)

}
