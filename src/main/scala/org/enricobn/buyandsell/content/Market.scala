package org.enricobn.buyandsell.content

import org.enricobn.consolegame.UpickleUtils
import org.enricobn.consolegame.content.SimpleSerializer
import org.enricobn.consolegame.fs.VirtualFileWithContent
import org.enricobn.shell.impl.VirtualShell
import org.enricobn.vfs.utils.Utils.RightBiasedEither
import org.enricobn.vfs.{Authentication, IOError}

object Market {
  private val NAME = "market"

  private val goodDefaultPrices = Map(
    "gold" -> BigDecimal(1000),
    "silver" -> BigDecimal(500),
    "bronze" -> BigDecimal(100)
  )

  def goodChanged(shell: VirtualShell, good: String, qty: Int): Either[IOError, Unit] = {
    implicit val authentication: Authentication = shell.authentication

    for {
      marketFileContent <- get(shell)
      _ <- marketFileContent.mapContent { market =>
        val actual = market.get(good)
        val newQty = actual.qty + qty
        val newPrice = if (newQty == 0) BigDecimal(0) else goodDefaultPrices(good) / BigDecimal(newQty)

        market.put(good, newQty, newPrice)
      }
    } yield ()
  }

  def get(shell: VirtualShell, good: String): Either[IOError, MarketEntry] = {
    implicit val authentication: Authentication = shell.authentication

    for {
      marketFileContent <- get(shell)
      market <- marketFileContent.content()
    } yield market.get(good)
  }

  def get(shell: VirtualShell): Either[IOError, VirtualFileWithContent[Market]] = {
    implicit val authentication: Authentication = shell.authentication

    for {
      home <- shell.homeFolder
      fileWithContent <- VirtualFileWithContent(classOf[Market], home, NAME, { () => Market(Map()) })
    } yield fileWithContent

  }

}

case class Market(entries: Map[String, MarketEntry]) {

  def put(good:String, qty: Int, price: BigDecimal): Market = copy(entries = entries + (good -> MarketEntry(qty, price)))

  def get(good: String) : MarketEntry = entries.getOrElse(good, MarketEntry(0, BigDecimal(0)))

  override def toString: String =
    entries.map(v => v._1 + "\t\t" + v._2.qty + "\t\t" + v._2.price + "$").mkString("\n")
}

object MarketSerializer extends SimpleSerializer(classOf[Market]) {
  override def serializeIt(content: Market): Either[IOError, String] = UpickleUtils.writeE(content)

  override def deserialize(ser: String): Either[IOError, Market] = UpickleUtils.readE[Market](ser)
}

case class MarketEntry(qty: Int, price: BigDecimal)
