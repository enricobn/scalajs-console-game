package org.enricobn.buyandsell.commands

import org.enricobn.buyandsell.content.Warehouse
import org.enricobn.consolegame.content.Messages
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

class SellCommand(private val messages: Messages) extends VirtualCommand {
  import SellCommand._

  private val arguments = new VirtualCommandArguments(
    new FileArgument("wareHouseFile", true, Some(getWarehouseFile(_).isDefined)),
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
        if (!shell.vum.checkWriteAccess(file)) {
          return "Access denied.".ioErrorE
        }
        val good = values(1).asInstanceOf[String]
        val qty = values(2).asInstanceOf[Int]

        for {
          warehouse <- contentAs(file, classOf[Warehouse]).right
          runContext <- warehouse.sell(good, qty)
            .toLeft({
              messages.add("sell " + qty + " of " + good)
              new RunContext()
            })
            .left
            .map(new IOError(_))
            .left
        } yield runContext
    }
  }

  override def completion(line: String, currentFolder: VirtualFolder): Seq[String] = {
    arguments.complete(currentFolder, line)
  }

  private def goodsProposals(file: VirtualFile, prefix: String) : Seq[String] = {
    getWarehouseFile(file) match {
      case Some(goods) => goods.goods.keySet.filter(_.startsWith(prefix)).toSeq
      case _ => Seq.empty
    }
  }

  private def getWarehouseFile(file: VirtualFile) : Option[Warehouse] =
    file.content.fold(_ => None, {
      case goods: Warehouse => Some(goods)
      case _ => None
    })

}