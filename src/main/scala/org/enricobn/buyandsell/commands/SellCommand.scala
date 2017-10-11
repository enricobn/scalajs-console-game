package org.enricobn.buyandsell.commands

import org.enricobn.buyandsell.content.Warehouse
import org.enricobn.consolegame.commands.MessagesCommand
import org.enricobn.shell._
import org.enricobn.shell.impl._
import org.enricobn.vfs.IOError._
import org.enricobn.vfs._

/**
  * Created by enrico on 12/17/16.
  */
object SellCommand {
  val NAME = "sell"
}

class SellCommand() extends VirtualCommand {
  import SellCommand._

  private val arguments = new VirtualCommandArguments(
    FileArgument("wareHouseFile", true, getWarehouseFile(_).isDefined),
    new StringArgument("good", true) {
      override def complete(currentFolder: VirtualFolder, value: String, previousArguments: Seq[Any]) =
        goodsProposals(previousArguments.head.asInstanceOf[VirtualFile], value)
    },
    IntArgument("qty", true)
  )

  override def getName: String = NAME

  override def run(shell: VirtualShell, shellInput: ShellInput, shellOutput: ShellOutput, args: String*) : Either[IOError, RunContext] = {
    arguments.parse(shell.currentFolder, "sell", args: _*) match {
      case Left(message) => ("sell: " + message).ioErrorE
      case Right(values) =>
        val file = values.head.asInstanceOf[VirtualFile]
        val good = values(1).asInstanceOf[String]
        val qty = values(2).asInstanceOf[Int]

        for {
          warehouse <- file.contentAs(classOf[Warehouse]).right
          newWarehouse <- warehouse.sell(good, qty).right
          _ <- (file.content = newWarehouse).toLeft(()).right
          _ <- MessagesCommand.addMessage(shell.currentFolder, "sell " + qty + " of " + good).toLeft(()).right
          runContext <- {
            Right(new RunContext()).right
          }
        } yield runContext
    }
  }

  override def completion(line: String, currentFolder: VirtualFolder): Seq[String] = {
    arguments.complete(currentFolder, line)
  }

  private def goodsProposals(file: VirtualFile, prefix: String) : Seq[String] = {
    getWarehouseFile(file) match {
      case Some(warehouse) => warehouse.goods.keySet.filter(_.startsWith(prefix)).toSeq
      case _ => Seq.empty
    }
  }

  private def getWarehouseFile(file: VirtualFile) : Option[Warehouse] =
    file.content.fold(_ => None, {
      case warehouse: Warehouse => Some(warehouse)
      case _ => None
    })

}
