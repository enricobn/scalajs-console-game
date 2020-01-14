package org.enricobn.buyandsell

import org.enricobn.buyandsell.commands.{CreateCityCommand, MainLoopCommand, SellCommand}
import org.enricobn.buyandsell.content._
import org.enricobn.consolegame.{ConsoleGame, GameCommand, Serializer}
import org.enricobn.shell.impl.VirtualShell
import org.enricobn.vfs.{Authentication, IOError}

import scala.scalajs.js.annotation.{JSExportAll, JSExportTopLevel}

object BuyAndSell {
  val serializers : Seq[Serializer] = List(GameStatisticsSerializer, CitySerializer, MarketSerializer,
    WarehouseSerializer, CityMapSerializer)
}

@JSExportTopLevel(name = "BuyAndSell")
@JSExportAll
class BuyAndSell(mainCanvasID: String, messagesCanvasID: String, newGameID: String, loadGameID: String, saveGameID: String)
extends ConsoleGame(mainCanvasID, messagesCanvasID, newGameID, loadGameID, saveGameID) {

  override def onNewGame(shell: VirtualShell): Option[IOError] = {
    val gameStatistics = GameStatistics(money = 10000, availableCities = 2, cities = Set.empty)

    val market = Market(Map("gold" -> 1000, "silver" -> 500, "bronze" -> 100))

    import org.enricobn.vfs.utils.Utils.RightBiasedEither
    implicit val authentication: Authentication = shell.authentication

    val job = for {
      home <- shell.homeFolder
      _ <- home.createFile("gamestats", gameStatistics)
      _ <- home.createFile("market", market)
    } yield ()

    job.left.toOption
  }

  override def commands: Either[IOError, Seq[GameCommand]] =
    Right(Seq(GameCommand(new SellCommand(), visible = true), GameCommand(new MainLoopCommand, visible = false),
      GameCommand(new CreateCityCommand, visible = true)))

  override def getSerializers: Seq[Serializer] = BuyAndSell.serializers

  override def getBackgroundCommand: Option[(String, List[String])] = Some((MainLoopCommand.name, List.empty))
}
