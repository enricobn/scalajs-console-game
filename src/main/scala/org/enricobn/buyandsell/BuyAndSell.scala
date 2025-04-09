package org.enricobn.buyandsell

import org.enricobn.buyandsell.commands.{BuyCommand, CreateCityCommand, MainLoopCommand}
import org.enricobn.buyandsell.content._
import org.enricobn.consolegame.{BrowserConsoleGame, GameCommand, Serializer}
import org.enricobn.shell.impl.VirtualShell
import org.enricobn.vfs.utils.Utils.RightBiasedEither
import org.enricobn.vfs.{Authentication, IOError}

import scala.scalajs.js.annotation.{JSExportAll, JSExportTopLevel}

object BuyAndSell {
  val serializers: Seq[Serializer] = List(GameStatisticsSerializer, GameInfoSerializer,
    WarehouseSerializer, CityMapSerializer)
}

@JSExportTopLevel(name = "BuyAndSell")
@JSExportAll
class BuyAndSell(mainCanvasID: String, messagesCanvasID: String, newGameID: String, loadGameID: String, saveGameID: String)
  extends BrowserConsoleGame(mainCanvasID, messagesCanvasID, newGameID, loadGameID, saveGameID) {

  override def onNewGame(shell: VirtualShell): Either[IOError, Unit] = {
    implicit val authentication: Authentication = shell.authentication

    val gameStatistics = GameStatistics(money = 10000, availableCities = 2, cities = Set.empty)

    val job = for {
      user1Shell <- createUserWithRandomPassword("user1")
      _ <- user1Shell.run("createcity", "User1City")
      user2Shell <- createUserWithRandomPassword("user2")
      _ <- user2Shell.run("createcity", "User2City")

      home <- shell.homeFolder
      _ <- home.createFile("gamestats", gameStatistics)
    } yield ()

    job
  }

  override def commands: Either[IOError, Seq[GameCommand]] =
    Right(Seq(GameCommand(BuyCommand, visible = true), GameCommand(MainLoopCommand, visible = false),
      GameCommand(CreateCityCommand, visible = true)))

  override def getSerializers: Seq[Serializer] = BuyAndSell.serializers

  override def getBackgroundCommand: Option[(String, List[String])] = Some((MainLoopCommand.name, List.empty))

}
