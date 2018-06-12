package org.enricobn.buyandsell.commands

import org.enricobn.buyandsell.content.Warehouse
import org.enricobn.consolegame.content.Messages
import org.enricobn.shell._
import org.enricobn.shell.impl._
import org.enricobn.vfs._

/**
  * Created by enrico on 12/17/16.
  */
object SellCommand {

  private val FILE = FileArgument("wareHouseFile", true, (folder, shell) => getWarehouseFile(folder)(shell.authentication).isDefined)

  private val GOOD = new StringArgument("good", true) {
    override def complete(shell: VirtualShell, value: String, previousArguments: Seq[Any]) =
      goodsProposals(previousArguments.head.asInstanceOf[VirtualFile], value)(shell.authentication)
  }

  private val QTY = IntArgument("qty", true)

  private def goodsProposals(file: VirtualFile, prefix: String)(implicit authentication: Authentication) : Seq[String] = {
    getWarehouseFile(file) match {
      case Some(warehouse) => warehouse.goods.keySet.filter(_.startsWith(prefix)).toSeq
      case _ => Seq.empty
    }
  }

  private def getWarehouseFile(file: VirtualFile)(implicit authentication: Authentication) : Option[Warehouse] =
    file.getContent.fold(_ => None, {
      case warehouse: Warehouse => Some(warehouse)
      case _ => None
    })
}


import org.enricobn.buyandsell.commands.SellCommand._

class SellCommand() extends VirtualCommandAbstract("sell", FILE, GOOD, QTY) {

  override def runParsed(shell: VirtualShell, shellInput: ShellInput, shellOutput: ShellOutput, args: Seq[Any])
                        (implicit authentication: Authentication)
  : Either[IOError, RunContext] = {
      val file = args.head.asInstanceOf[VirtualFile]
      val good = args(1).asInstanceOf[String]
      val qty = args(2).asInstanceOf[Int]

      for {
        warehouse <- file.contentAs(classOf[Warehouse]).right
        newWarehouse <- warehouse.sell(good, qty).right
        _ <- file.setContent(newWarehouse).toLeft(()).right
        _ <- Messages.addMessage(shell, "sell " + qty + " of " + good).toLeft(()).right
      } yield new RunContext()

  }

}
