package org.enricobn.buyandsell.content

import scala.collection.mutable

/**
  * Created by enrico on 1/8/17.
  */
case class CityMap(width: Int, height: Int, buildings: Map[Position, Building] = Map.empty) {

  def add(position: Position, building: Building): Either[String,CityMap] = {
    if (position.x < 0 || position.y < 0) {
      return Left("Out of bounds.")
    }

    for (x <- 0 until building.width) {
      for (y <- 0 until building.height) {
        val cell: Position = Position(position.x + x, position.y + y)
        if (buildings.get(cell).isDefined) {
          return Left(s"$cell Already defined.")
        } else if (cell.x >= width || cell.y >= height) {
          return Left("Out of bounds.")
        }
      }
    }

    val newBuildings = mutable.Map(buildings.toSeq: _*)

    for (x <- 0 until building.width) {
      for (y <- 0 until building.height) {
        newBuildings += (Position(position.x + x, position.y + y) -> building)
      }
    }
    Right(CityMap(width, height, newBuildings.toMap))
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