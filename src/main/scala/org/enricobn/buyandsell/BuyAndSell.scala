package org.enricobn.buyandsell

import org.enricobn.buyandsell.commands.SellCommand
import org.enricobn.buyandsell.content._
import org.enricobn.consolegame.{ConsoleGame, Serializer}
import org.enricobn.shell.VirtualCommand
import org.enricobn.vfs.IOError

import scala.scalajs.js.annotation.{JSExport, JSExportAll}

@JSExport(name = "BuyAndSell")
@JSExportAll
class BuyAndSell(mainCanvasID: String, messagesCanvasID: String, loadGameID: String, saveGameID: String)
extends ConsoleGame(mainCanvasID, messagesCanvasID, loadGameID, saveGameID) {

  override def onNewGame(): Option[IOError] = {
    val gameStatistics = GameStatistics(10000)
    val city = City("Pisa", Statistics(100, 0))
    val market = Market(Map("gold" -> 1000, "silver" -> 500, "bronze" -> 100))
    val warehouse = Warehouse(Map("gold" -> 2, "silver" -> 10, "bronze" -> 20))

    val job = for {
      guest <- root.resolveFolderOrError("/home/guest", "Cannot find folder /home/guest.").right
      gameStatisticsFile <- guest.touch("gamestats").right
      _ <- (gameStatisticsFile.content = gameStatistics).toLeft(None).right
      marketFile <- guest.touch("market").right
      _ <- (marketFile.content = market).toLeft(None).right
      cityFolder <- guest.mkdir("Pisa").right
      cityFile <- cityFolder.touch("city").right
      _ <- (cityFile.content = city).toLeft(None).right
      warehouseFile <- cityFolder.touch("warehouse").right
      result <- (warehouseFile.content = warehouse).toLeft(None).right
    } yield result

    job.left.toOption

  }

  def getCommands(): Either[IOError, Seq[VirtualCommand]] =
    Right(Seq(new SellCommand()))

  override def getSerializers: Seq[Serializer] =
    List(GameStatisticsSerializer, CitySerializer, MarketSerializer, WarehouseSerializer)

}
