package org.enricobn.consolegame.content

import scala.collection.mutable

/**
  * Created by enrico on 12/17/16.
  */
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
