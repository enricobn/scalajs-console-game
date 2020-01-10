package org.enricobn.buyandsell.content

import org.enricobn.consolegame.UpickleUtils
import org.enricobn.consolegame.content.SimpleSerializer
import org.enricobn.shell.impl.VirtualShell
import org.enricobn.vfs.utils.Utils.RightBiasedEither
import org.enricobn.vfs.{Authentication, IOError}

object Market {

  def plus(shell: VirtualShell, key: String, value: BigDecimal) : Either[IOError, Unit] = {
    implicit val authentication: Authentication = shell.authentication

    for {
      home <- shell.homeFolder
      marketFile <- home.findFileOrError("market", "Cannot find market file.")
      market <- marketFile.contentAs(classOf[Market])
      actual = market.prices.withDefault(_ => BigDecimal(0))(key)
      newMarket = market.copy(prices = market.prices + (key -> (actual + value)))
      _ <- marketFile.setContent(newMarket).toLeft(())
    } yield ()
  }

  def get(shell: VirtualShell): Either[IOError, Market] = {
    implicit val authentication: Authentication = shell.authentication

    for {
      home <- shell.homeFolder
      marketFile <- home.findFileOrError("market", "Cannot find market file.")
      market <- marketFile.contentAs(classOf[Market])
    } yield market
  }

}

case class Market(prices: Map[String,BigDecimal]) {

  override def toString: String =
    prices.map(v => v._1 + "\t\t" + v._2 + "$").mkString("\n")
}

object MarketSerializer extends SimpleSerializer(classOf[Market]) {
  override def serializeIt(content: Market): Either[IOError, String] = UpickleUtils.writeE(content)

  override def deserialize(ser: String): Either[IOError, Market] = UpickleUtils.readE[Market](ser)
}
