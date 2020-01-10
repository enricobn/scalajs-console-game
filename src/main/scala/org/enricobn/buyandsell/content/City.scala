package org.enricobn.buyandsell.content

import org.enricobn.consolegame.UpickleUtils
import org.enricobn.consolegame.content.SimpleSerializer
import org.enricobn.shell.impl.VirtualShell
import org.enricobn.vfs.utils.Utils.RightBiasedEither
import org.enricobn.vfs.{Authentication, IOError}
import upickle.default._

object City {

  def get(shell: VirtualShell, cityName: String): Either[IOError, City] = {
    implicit val authentication: Authentication = shell.authentication

    for {
      home <- shell.homeFolder
      cityFile <- home.resolveFileOrError(List(cityName, "city"))
      city <- cityFile.contentAs(classOf[City])
    } yield city
  }

}

/**
  * Created by enrico on 1/8/17.
  */
case class City(name: String, statistics: Statistics) {

  override def toString: String =
    "Name: " + name + "\n" +
    statistics
}

case class Statistics(population: Int, employed: Int) {

  override def toString: String =
    "Population: " + population + "\n" +
    "Employed: " + employed
}

object CitySerializer extends SimpleSerializer(classOf[City]) {

  override def serializeIt(content: City): Either[IOError, String] = UpickleUtils.writeE(content)

  override def deserialize(ser: String): Either[IOError, City] = UpickleUtils.readE[City](ser)

}