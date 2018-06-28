package org.enricobn.buyandsell

import org.enricobn.buyandsell.commands.SellCommand
import org.enricobn.buyandsell.content._
import org.enricobn.consolegame.{ConsoleGame, Serializer}
import org.enricobn.shell.VirtualCommand
import org.enricobn.shell.impl.VirtualShell
import org.enricobn.vfs.{Authentication, IOError}

import scala.scalajs.js.annotation.{JSExport, JSExportAll}

object BuyAndSell {
  val serializers : Seq[Serializer] = List(GameStatisticsSerializer, CitySerializer, MarketSerializer, WarehouseSerializer)
}

@JSExport(name = "BuyAndSell")
@JSExportAll
class BuyAndSell(mainCanvasID: String, messagesCanvasID: String, newGameID: String, loadGameID: String, saveGameID: String)
extends ConsoleGame(mainCanvasID, messagesCanvasID, newGameID, loadGameID, saveGameID) {

  override def onNewGame(shell: VirtualShell): Option[IOError] = {
    implicit val authentication: Authentication = shell.authentication

    val gameStatistics = GameStatistics(10000)
    val city = City("Pisa", Statistics(100, 0))
    val market = Market(Map("gold" -> 1000, "silver" -> 500, "bronze" -> 100))
    val warehouse = Warehouse(Map("gold" -> 2, "silver" -> 10, "bronze" -> 20))

    val job = for {
      home <- shell.homeFolder.right
      _ <- home.createFile("gamestats", gameStatistics).right
      _ <- home.createFile("market", market).right
      cityFolder <- home.mkdir("Pisa").right
      _ <- cityFolder.createFile("city", city).right
      result <- cityFolder.createFile("warehouse", warehouse).right
    } yield result

    job.left.toOption

  }

  override def commands: Either[IOError, Seq[VirtualCommand]] =
    Right(Seq(new SellCommand()))

  override def getSerializers: Seq[Serializer] = BuyAndSell.serializers

}
