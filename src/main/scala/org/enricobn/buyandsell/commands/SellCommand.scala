package org.enricobn.buyandsell.commands

import org.enricobn.buyandsell.content.{GameStatistics, Market, Warehouse}
import org.enricobn.consolegame.content.Messages
import org.enricobn.shell._
import org.enricobn.shell.impl._
import org.enricobn.vfs._
import org.enricobn.vfs.utils.Utils.RightBiasedEither

/**
  * Created by enrico on 12/17/16.
  */
private object SellCommandArguments {

  val FILE = FileArgument("wareHouseFile", required = true, (folder, shell) => getWarehouseFile(folder)(shell.authentication).isDefined)

  val GOOD: StringArgument = new StringArgument("good", required = true) {
    override def complete(shell: VirtualShell, value: String, previousArguments: Seq[Any]): Seq[String] =
      goodsProposals(previousArguments.head.asInstanceOf[VirtualFile], value)(shell.authentication)
  }

  val QTY = IntArgument("qty", required = true)

  def goodsProposals(file: VirtualFile, prefix: String)(implicit authentication: Authentication) : Seq[String] = {
    getWarehouseFile(file) match {
      case Some(warehouse) => warehouse.goods.keySet.filter(_.startsWith(prefix)).toSeq
      case _ => Seq.empty
    }
  }

  def getWarehouseFile(file: VirtualFile)(implicit authentication: Authentication) : Option[Warehouse] =
    file.getContent.fold(_ => None, {
      case warehouse: Warehouse => Some(warehouse)
      case _ => None
    })
}

import org.enricobn.buyandsell.commands.SellCommandArguments._

object SellCommand extends VirtualCommandAbstract("sell", FILE, GOOD, QTY) {

  override def runParsed(shell: VirtualShell, shellInput: ShellInput, shellOutput: ShellOutput, args: Seq[Any])
                        (implicit authentication: Authentication)
  : Either[IOError, VirtualProcess] = {
      val file = args.head.asInstanceOf[VirtualFile]
      val good = args(1).asInstanceOf[String]
      val qty = args(2).asInstanceOf[Int]

      for {
        warehouse <- file.contentAs(classOf[Warehouse])
        gamestatsFile <- GameStatistics.apply(shell)
        marketEntry <- Market.get(shell, good)
        _ <- gamestatsFile.mapContent(_.add(marketEntry.price * qty))
        newWarehouse <- Right(warehouse.add(shell, good, -qty))
        _ <- file.setContent(newWarehouse)
        _ <- Messages.addMessage(shell, "sell " + qty + " of " + good)
      } yield new VirtualProcess()

  }

}
