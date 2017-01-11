package org.enricobn.consolegame.content

import scala.collection.mutable

/**
  * Created by enrico on 1/8/17.
  */
class CityMap(width: Int, height: Int) {
  private val buildings = new mutable.HashMap[Position, Building]()

  def add(position: Position, building: Building): Boolean = {
    if (position.x < 0 || position.y < 0) {
      return false
    }

    for (x <- 0 until building.width) {
      for (y <- 0 until building.height) {
        val cell: Position = Position(position.x + x, position.y + y)
        if (buildings.get(cell).isDefined) {
          return false
        } else if (cell.x >= width || cell.y >= height) {
          return false
        }
      }
    }

    for (x <- 0 until building.width) {
      for (y <- 0 until building.height) {
        buildings.put(Position(position.x + x, position.y + y), building)
      }
    }
    true
  }

  override def toString: String = {
    val sb = new StringBuilder()

    for (y <- 0 until height) {
      addRow(sb)
      for (x <- 0 until width) {
        sb.append('|')
        buildings.get(Position(x, y)) match {
          case Some(b) => sb.append(b.id)
          case _ => sb.append(' ')
        }
      }
      sb.append("|\n")
    }
    addRow(sb)
    sb.toString()
  }

  private def addRow(sb: StringBuilder): Unit = {
    for (x <- 0 until width) {
      sb.append("+-")
    }
    sb.append("+\n")
  }
}

case class Position(x: Int, y: Int)

sealed trait Building {
  val width: Int
  val height: Int
  val id: Char
  val size: Int = width * height
}

case class PoliceStation(width: Int, height: Int) extends Building {
  override val id: Char = 'P'
}

case class Hospital(width: Int, height: Int) extends Building {
  override val id: Char = 'H'
}