package org.enricobn.buyandsell

import org.enricobn.buyandsell.commands.SellCommand
import org.enricobn.buyandsell.content.{Warehouse, WarehouseSerializer}
import org.enricobn.consolegame.{ConsoleGame, Serializer}
import org.enricobn.shell.VirtualCommand
import org.enricobn.vfs.IOError

import scala.scalajs.js.annotation.{JSExport, JSExportAll}

@JSExport(name = "BuyAndSell")
@JSExportAll
class BuyAndSell(mainCanvasID: String, messagesCanvasID: String, loadGameID: String, saveGameID: String)
extends ConsoleGame(mainCanvasID, messagesCanvasID, loadGameID, saveGameID) {

  override def onNewGame(): Option[IOError] = {
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

  def getCommands(): Either[IOError, Seq[VirtualCommand]] =
    Right(Seq(new SellCommand()))

  override def getSerializers() : Seq[Serializer] =
    List(WarehouseSerializer)

}
