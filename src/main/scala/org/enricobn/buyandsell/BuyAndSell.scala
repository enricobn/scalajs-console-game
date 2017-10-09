package org.enricobn.buyandsell

import org.enricobn.buyandsell.commands.SellCommand
import org.enricobn.buyandsell.content.{Warehouse, WarehouseSerializer}
import org.enricobn.consolegame.commands.MessagesCommand
import org.enricobn.consolegame.content.Messages
import org.enricobn.consolegame.{ConsoleGame, Serializer}
import org.enricobn.vfs.{IOError, VirtualFile}

import scala.scalajs.js.annotation.{JSExport, JSExportAll}

@JSExport(name = "BuyAndSell")
@JSExportAll
class BuyAndSell(mainCanvasID: String, messagesCanvasID: String, loadGameID: String, saveGameID: String)
extends ConsoleGame(mainCanvasID, messagesCanvasID, loadGameID, saveGameID) {

  override def initGame(): Option[IOError] = {
    val warehouse = new Warehouse()
    warehouse.add("gold", 2)
    warehouse.add("silver", 10)
    warehouse.add("bronze", 20)

    val job = for {
      guest <- root.resolveFolderOrError("/home/guest", "Cannot find folder /home/guest.").right
      warehouseFile <- guest.touch("warehouse").right
      _ <- warehouseFile.chown("guest").toLeft(None).right
      result <- (warehouseFile.content = warehouse).toLeft(None).right
    } yield result

    job.left.toOption

  }

  def createUserCommands(): Either[IOError,Seq[VirtualFile]] =
    for {
      usrBin <- root.resolveFolderOrError("/usr/bin", "Cannot find folder /usr/bin.").right
      log <- root.resolveFolderOrError("/var/log", "Cannot find folder /var/log.").right
      messagesFile <- log.findFileOrError("messages.log", "Cannot find file /var/log/messages.log.").right
      messages <- messagesFile.content.right
      sellCommand <- context.createCommandFile(usrBin, new SellCommand(messages.asInstanceOf[Messages])).right
      messagesCommand <- context.createCommandFile(usrBin, new MessagesCommand(messages.asInstanceOf[Messages])).right
    } yield List(sellCommand,messagesCommand)

  override def getSerializers() : Seq[Serializer] =
    List(WarehouseSerializer)

}
