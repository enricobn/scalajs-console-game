package org.enricobn.buyandsell.content

import org.enricobn.buyandsell.content.GoodEnum.GoodEnum
import org.enricobn.consolegame.UpickleUtils
import org.enricobn.consolegame.content.SimpleSerializer
import org.enricobn.shell.impl.VirtualShell
import org.enricobn.vfs.IOError._
import org.enricobn.vfs.utils.Utils.RightBiasedEither
import org.enricobn.vfs.{Authentication, IOError}

object Warehouse {

  def get(shell: VirtualShell, cityName: String): Either[IOError, Warehouse] = {
    implicit val authentication: Authentication = shell.authentication

    for {
      home <- shell.homeFolder
      warehouseFile <- home.resolveFileOrError(List(cityName, "warehouse"))
      warehouse <- warehouseFile.contentAs(classOf[Warehouse])
    } yield warehouse
  }

}

/**
  * Created by enrico on 12/17/16.
  */
case class Warehouse(private val goods: List[Good]) {

  def estimate(goodEnum: GoodEnum, qty: Int): BigDecimal = ???

  def change(goodEnum: GoodEnum, qty: Int): Either[IOError, Warehouse] = {
    // TODO handle error
    //Market.goodChanged(shell, goodEnum, qty)
    goods.find(_.good == goodEnum.toString) match {
      case Some(v) if qty > 0 || -qty <= v.qty => Right(Warehouse(Good(goodEnum.toString, qty + v.qty, v.price) :: goods.filter(_.good != goodEnum.toString)))
      case Some(_) => "Not enough quantity.".ioErrorE
      case None if qty > 0 => Right(Warehouse(Good(goodEnum.toString, qty, None) :: goods))
      case None => "Cannot find good.".ioErrorE
    }
  }

  def setPrice(goodEnum: GoodEnum, price: Price): Either[IOError, Warehouse] = {
    goods.find(_.good == goodEnum.toString) match {
      case Some(good) => Right(copy(goods = good.copy(price = Some(price)) :: goods.filter(_.good != goodEnum.toString)))
      case _ => "Cannot find good.".ioErrorE
    }
  }

  def getPrice(goodEnum: GoodEnum): Option[Price] =
    goods.find(_.good == goodEnum.toString).flatMap(_.price)

  def availableGoodNames: List[String] = goods.filter(good => good.qty > 0 && good.price.nonEmpty).map(_.good.toString)

  override def toString: String = {
    goods.map(v => v.good + "\t\t" + v.qty + "\t\t" + toString(v.price)).mkString("\n")
  }

  private def toString(price: Option[Price]): String = {
    price.map(_.base.toString).getOrElse("-")
  }
}

case class Good(good: String, qty: Int, price: Option[Price])

case class Price(base: BigDecimal)

object WarehouseSerializer extends SimpleSerializer(classOf[Warehouse]) {

  override def serializeIt(content: Warehouse): Either[IOError, String] = UpickleUtils.writeE(content)

  override def deserialize(ser: String): Either[IOError, Warehouse] = UpickleUtils.readE[Warehouse](ser)
}