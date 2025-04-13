package org.enricobn.buyandsell.content

import org.enricobn.consolegame.UpickleUtils
import org.enricobn.consolegame.content.SimpleSerializer
import org.enricobn.vfs.IOError
import upickle.default.{macroRW, ReadWriter as RW}

import scala.collection.mutable

/**
  * Created by enrico on 1/8/17.
  */

object CityMap {
  implicit val rw: RW[CityMap] = macroRW
}

case class CityMap(width: Int, height: Int, buildings: Map[Position, Building] = Map.empty) {

  def add(position: Position, building: Building): Either[String, CityMap] = {
    if (position.x < 0 || position.y < 0) {
      return Left("Out of bounds.")
    }

    for (x <- 0 until building.width) {
      for (y <- 0 until building.height) {
        val cell: Position = Position(position.x + x, position.y + y)
        if (buildings.contains(cell)) {
          return Left(s"$cell Already defined.")
        } else if (cell.x >= width || cell.y >= height) {
          return Left("Out of bounds.")
        }
      }
    }

    val newBuildings = mutable.Map(buildings.toSeq *)

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
    sb.append("+-" * width)
    sb.append("+\n")
  }
}

object Position {
  implicit val rw: RW[Position] = macroRW
}

case class Position(x: Int, y: Int)

object Building {
  implicit val rw: RW[Building] = RW.merge(PoliceStation.rw, Hospital.rw)
}

sealed trait Building {
  val width: Int
  val height: Int
  val id: Char
  val size: Int = width * height
}

object PoliceStation {
  implicit val rw: RW[PoliceStation] = macroRW
}

case class PoliceStation(width: Int, height: Int) extends Building {
  override val id: Char = 'P'
}

object Hospital {
  implicit val rw: RW[Hospital] = macroRW
}

case class Hospital(width: Int, height: Int) extends Building {
  override val id: Char = 'H'
}

object CityMapSerializer extends SimpleSerializer(classOf[CityMap]) {

  override def serializeIt(content: CityMap): Either[IOError, String] = UpickleUtils.writeE(content)

  override def deserialize(ser: String): Either[IOError, CityMap] = UpickleUtils.readE[CityMap](ser)

}