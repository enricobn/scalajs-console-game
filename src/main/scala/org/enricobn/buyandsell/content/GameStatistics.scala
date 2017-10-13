package org.enricobn.buyandsell.content

import org.enricobn.consolegame.UpickleUtils
import org.enricobn.consolegame.content.SimpleSerializer
import org.enricobn.vfs.IOError

case class GameStatistics(money: Int) {
  override def toString = s"Money: $money"
}

object GameStatisticsSerializer extends SimpleSerializer(classOf[GameStatistics]) {
  override def serializeIt(content: GameStatistics): Either[IOError, String] = UpickleUtils.writeE(content)

  override def deserialize(ser: String): Either[IOError, GameStatistics] = UpickleUtils.readE[GameStatistics](ser)
}
