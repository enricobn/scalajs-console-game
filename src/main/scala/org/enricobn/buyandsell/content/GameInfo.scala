package org.enricobn.buyandsell.content

import org.enricobn.consolegame.UpickleUtils
import org.enricobn.consolegame.content.SimpleSerializer
import org.enricobn.vfs._
import org.enricobn.vfs.utils.Utils.RightBiasedEither
import upickle.default._

object GameInfo {

  def get(fs: VirtualFS)(implicit authentication: Authentication): Either[IOError, VirtualFileWithContent[GameInfo]] = {
    for {
      path <- VirtualPath.absolute("etc", "gameinfo")
      fileWithContent <- Right(new VirtualFileWithContent(classOf[GameInfo], fs, path))
    } yield fileWithContent
  }

}

case class GameInfo(user: String)

object GameInfoSerializer extends SimpleSerializer(classOf[GameInfo]) {

  override def serializeIt(content: GameInfo): Either[IOError, String] = UpickleUtils.writeE(content)

  override def deserialize(ser: String): Either[IOError, GameInfo] = UpickleUtils.readE[GameInfo](ser)

}