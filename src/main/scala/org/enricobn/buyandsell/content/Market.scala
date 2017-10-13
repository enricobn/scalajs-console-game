package org.enricobn.buyandsell.content

import org.enricobn.consolegame.UpickleUtils
import org.enricobn.consolegame.content.SimpleSerializer
import org.enricobn.vfs.{IOError, VirtualPath}

object Market {
  val marketPath = VirtualPath("~/market")
}

case class Market(prices: Map[String,Double]) {
  override def toString: String =
    prices.map(v => v._1 + "\t\t" + v._2 + "$").mkString("\n")
}

object MarketSerializer extends SimpleSerializer(classOf[Market]) {
  override def serializeIt(content: Market): Either[IOError, String] = UpickleUtils.writeE(content)

  override def deserialize(ser: String): Either[IOError, Market] = UpickleUtils.readE[Market](ser)
}
