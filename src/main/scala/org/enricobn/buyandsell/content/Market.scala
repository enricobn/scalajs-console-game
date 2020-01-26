package org.enricobn.buyandsell.content

import org.enricobn.consolegame.UpickleUtils
import org.enricobn.consolegame.content.SimpleSerializer
import org.enricobn.shell.impl.VirtualShell
import org.enricobn.vfs.utils.Utils.RightBiasedEither
import org.enricobn.vfs.{Authentication, IOError, VirtualFileWithContent, VirtualPath}

import scala.math.BigDecimal.RoundingMode

object Market {
  private val NAME = "market"

  def goodChanged(shell: VirtualShell, good: String, qty: Int): Either[IOError, Unit] = {
    implicit val authentication: Authentication = shell.authentication

    for {
      marketFileContent <- get(shell)
      _ <- marketFileContent.mapContent { market => market.goodChanged(good, qty) }
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
      gameInfo <- GameInfo.get(shell.fs).flatMap(_.content())
      path <- VirtualPath.of("home", gameInfo.user, NAME)
    } yield new VirtualFileWithContent(classOf[Market], shell.fs, path)

  }

}

case class Market(defaultPrices: Map[String, BigDecimal], entries: Map[String, MarketEntry]) {

  def addDefaultPrice(good: String, price: BigDecimal): Market = copy(defaultPrices = defaultPrices + (good -> price))

  def goodChanged(good: String, qty: Int): Market = {
    val actual = get(good)
    val newQty = actual.qty + qty
    val newPrice = if (newQty == 0) BigDecimal(0) else (defaultPrices(good) / BigDecimal(newQty)).setScale(2, RoundingMode.HALF_DOWN)

    put(good, newQty, newPrice)
  }

  def put(good:String, qty: Int, price: BigDecimal): Market = copy(entries = entries + (good -> MarketEntry(qty, price)))

  def get(good: String) : MarketEntry = entries.getOrElse(good, MarketEntry(0, BigDecimal(0)))

  override def toString: String = {
    entries.map(v => v._1 + "\t\t" + v._2.qty + "\t\t" + format(v._2.price) + "$").mkString("\n")
  }

  private def format(bigDecimal: BigDecimal) = {
    val long = bigDecimal.toLong
    val decimals = ((bigDecimal - long) * 100).toLong
    val stringBuilder = StringBuilder.newBuilder
    if (bigDecimal.signum < 0) {
      stringBuilder.append("-")
    }
    stringBuilder.append(long)
      .append(".")
      .append(decimals)
  }
}

object MarketSerializer extends SimpleSerializer(classOf[Market]) {
  override def serializeIt(content: Market): Either[IOError, String] = UpickleUtils.writeE(content)

  override def deserialize(ser: String): Either[IOError, Market] = UpickleUtils.readE[Market](ser)
}

case class MarketEntry(qty: Int, price: BigDecimal)
