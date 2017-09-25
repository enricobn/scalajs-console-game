package org.enricobn.buyandsell.content

import org.enricobn.consolegame.{GameState, Serializer}
import org.enricobn.vfs.IOError
import upickle.Js
import upickle.default._

import scala.collection.mutable
import scala.scalajs.js

/**
  * Created by enrico on 12/17/16.
  */
object Warehouse {
  implicit val writer = upickle.default.Writer[Warehouse] {
    case t => writeJs(t.goods.toMap)
  }

  implicit val reader = upickle.default.Reader[Warehouse] {
    case o: Js.Obj =>
      val warehouse = new Warehouse()
      o.value.foreach(v => {
        v._2 match {
          case Js.Num(d) => warehouse.add(v._1, d.toInt)
          case _ => throw new IllegalArgumentException(v._2 + " is not a Json double.")
        }
      })
      warehouse
  }
}

class Warehouse {
  val goods = new mutable.HashMap[String, Int]()

  def add(good: String, qty:Int): Unit = {
    goods.get(good) match {
      case Some(v) => goods.put(good, qty + v)
      case _ => goods.put(good, qty)
    }
  }

  def sell(good: String, qty:Int): Option[String] = {
    goods.get(good) match {
      case Some(v) if v > qty =>
        goods.put(good, v - qty)
        None
      case Some(v) if v == qty =>
        goods.remove(good)
        None
      case Some(v) =>
        Some("Invalid qty.")
      case _ =>
        Some("Cannot find good.")
    }
  }

  override def toString: String = {
    goods.map(v => v._1 + "\t\t" + v._2).mkString("\n")
  }
}

object WarehouseSerializer extends Serializer {
  private val writer = upickle.default.Writer[Warehouse](t => writeJs(t.goods.toMap))

  private val reader = upickle.default.Reader[Warehouse] {
    case o: Js.Obj =>
      val warehouse = new Warehouse()
      o.value.foreach(v => {
        v._2 match {
          case Js.Num(d) => warehouse.add(v._1, d.toInt)
          case _ => throw new IllegalArgumentException(v._2 + " is not a Json double.")
        }
      })
      warehouse
  }

  override val name = "Warehouse"

  override val clazz: Class[_] = classOf[Warehouse]

  override def serialize(content: AnyRef): Either[IOError, String] =
    content match {
      case warehouse: Warehouse =>
        Right(writer.write(warehouse).toString())
      case _ => Left(IOError("Not an instance of Warehouse."))
    }

  override def deserialize(ser: String): Either[IOError, Warehouse] =
    try {
      Right(reader.read(upickle.json.read(ser)))
    } catch {
      case e: Exception => Left(IOError(e.getMessage))
    }

}
