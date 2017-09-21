package org.enricobn.buyandsell

import org.enricobn.consolegame.ConsoleGame
import org.enricobn.consolegame.content.{Messages, Warehouse}
import org.enricobn.vfs.IOError

import scala.scalajs.js.annotation.{JSExport, JSExportAll}

@JSExport(name = "BuyAndSell")
@JSExportAll
class BuyAndSell(mainCanvasID: String, messagesCanvasID: String, loadGameID: String, saveGameID: String)
  extends ConsoleGame[BuyAndSellGameState, BuyAndSellSerializableGameState, BuyAndSellGameStateFactory.type](mainCanvasID, messagesCanvasID, loadGameID, saveGameID, BuyAndSellGameStateFactory)
{
  override def newGame(gameState: BuyAndSellGameState): Option[IOError] = {
    val warehouse = new Warehouse()
    warehouse.add("gold", 2)
    warehouse.add("silver", 10)
    warehouse.add("bronze", 20)

    val messages = new Messages()

    val job = for {
      log <- root.resolveFolderOrError("/var/log", "Cannot find folder /var/log.").right
      guest <- root.resolveFolderOrError("/home/guest", "Cannot find folder /home/guest.").right
      warehouseFile <- guest.touch("warehouse").right
      _ <- warehouseFile.chown("guest").toLeft(None).right
      _ <- (warehouseFile.content = warehouse).toLeft(None).right
      messagesFile <- log.touch("messages.log").right
      _ <- (messagesFile.content = messages).toLeft(None).right
    } yield {
      gameState.setMessages(messagesFile, messages)
      gameState.add(warehouseFile, warehouse)
    }

    job.left.toOption

  }
}
