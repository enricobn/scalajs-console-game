package org.enricobn.buyandsell.content

import org.enricobn.consolegame.UpickleUtils
import org.enricobn.consolegame.content.SimpleSerializer
import org.enricobn.vfs.IOError
import org.enricobn.vfs.IOError._

/**
  * Created by enrico on 12/17/16.
  */
case class Warehouse(goods: Map[String, Int]) {

  def add(good: String, qty:Int): Warehouse = {
    goods.get(good) match {
      case Some(v) => Warehouse(goods + (good -> (qty + v)))
      case _ => Warehouse(goods + (good -> qty))
    }
  }

  def sell(good: String, qty:Int): Either[IOError,Warehouse] = {
    goods.get(good) match {
      case Some(v) if v > qty =>
        Right(Warehouse(goods + (good -> (v - qty))))
      case Some(v) if v == qty =>
        Right(Warehouse(goods - good))
      case Some(v) =>
        "Invalid qty.".ioErrorE
      case _ =>
        "Cannot find good.".ioErrorE
    }
  }

  override def toString: String = {
    goods.map(v => v._1 + "\t\t" + v._2).mkString("\n")
  }
}

object WarehouseSerializer extends SimpleSerializer(classOf[Warehouse]) {

  override def serializeIt(content: Warehouse): Either[IOError, String] = UpickleUtils.writeE(content)

  override def deserialize(ser: String): Either[IOError, Warehouse] = UpickleUtils.readE[Warehouse](ser)
}