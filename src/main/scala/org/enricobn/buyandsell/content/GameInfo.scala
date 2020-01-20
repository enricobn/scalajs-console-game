package org.enricobn.buyandsell.content

import org.enricobn.consolegame.UpickleUtils
import org.enricobn.consolegame.content.SimpleSerializer
import org.enricobn.vfs.utils.Utils.RightBiasedEither
import org.enricobn.vfs.{Authentication, IOError, VirtualFS, VirtualFileWithContent}
import upickle.default._

object GameInfo {

  def get(fs: VirtualFS)(implicit authentication: Authentication): Either[IOError, VirtualFileWithContent[GameInfo]] = {
    for {
      etc <- fs.root.resolveFolderOrError("etc")
      file <- etc.findFileOrError("gameinfo", "cannot find gameinfo file")
      fileWithContent <- Right(new VirtualFileWithContent(classOf[GameInfo], file))
    } yield fileWithContent
  }

  def getOrCreate(fs: VirtualFS, createFun: () => GameInfo)(implicit authentication: Authentication): Either[IOError, VirtualFileWithContent[GameInfo]] = {
    for {
      etc <- fs.root.resolveFolderOrError("etc")
      fileWithContent <- VirtualFileWithContent.getOrCreate(classOf[GameInfo], etc, "gameinfo", createFun)
    } yield fileWithContent
  }

}

case class GameInfo(user: String)

object GameInfoSerializer extends SimpleSerializer(classOf[GameInfo]) {

  override def serializeIt(content: GameInfo): Either[IOError, String] = UpickleUtils.writeE(content)

  override def deserialize(ser: String): Either[IOError, GameInfo] = UpickleUtils.readE[GameInfo](ser)

}