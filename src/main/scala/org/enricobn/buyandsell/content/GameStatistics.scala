package org.enricobn.buyandsell.content

import org.enricobn.consolegame.UpickleUtils
import org.enricobn.consolegame.content.SimpleSerializer
import org.enricobn.shell.impl.VirtualShell
import org.enricobn.vfs.utils.Utils.RightBiasedEither
import org.enricobn.vfs.{Authentication, IOError, VirtualFileWithContent, VirtualPath}

case class GameStatistics(money: BigDecimal, availableCities: Int, cities: Set[String]) {

  def add(money: BigDecimal): GameStatistics = copy(money = this.money + money)

  override def toString = s"Money: $money\ncities:$cities"
}

object GameStatistics {

  def apply(shell: VirtualShell) : Either[IOError, VirtualFileWithContent[GameStatistics]] = {
    implicit val authentication: Authentication = shell.authentication

    for {
      home <- shell.homeFolder
      path = VirtualPath(home.path)
    } yield new VirtualFileWithContent(classOf[GameStatistics], shell.fs, path.andThen("gamestats"))
  }

}

object GameStatisticsSerializer extends SimpleSerializer(classOf[GameStatistics]) {
  override def serializeIt(content: GameStatistics): Either[IOError, String] = UpickleUtils.writeE(content)

  override def deserialize(ser: String): Either[IOError, GameStatistics] = UpickleUtils.readE[GameStatistics](ser)
}
