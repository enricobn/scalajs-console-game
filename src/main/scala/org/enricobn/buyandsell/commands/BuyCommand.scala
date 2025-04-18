package org.enricobn.buyandsell.commands

import org.enricobn.buyandsell.content.EitherEnumeration.EitherEnumeration
import org.enricobn.buyandsell.content.{GameStatistics, GoodEnum, Warehouse}
import org.enricobn.consolegame.content.Messages
import org.enricobn.shell.*
import org.enricobn.shell.impl.*
import org.enricobn.vfs.*

import scala.util.{Failure, Success, Try}

/**
  * Created by enrico on 12/17/16.
  */
private object BuyCommandArguments {

  val FROM: FileArgument = FileArgument("'from warehouse'", required = true,
    (folder, shell) => getWarehouseFile(folder)(shell.authentication).isDefined
  )

  val GOOD: StringArgument = new StringArgument("good", required = true) {
    override def complete(shell: VirtualShell, value: String, previousArguments: Seq[Any]): Seq[Completion] =
      goodsProposals(previousArguments.head.asInstanceOf[VirtualFile], value)(shell.authentication)
  }

  val QTY: IntArgument = IntArgument("quantity", required = true)

  val TO: FileArgument = FileArgument("'to warehouse'", required = true,
    (folder, shell) => getWarehouseFile(folder)(shell.authentication).isDefined
  )

  def goodsProposals(file: VirtualFile, prefix: String)(implicit authentication: Authentication) : Seq[Completion] = {
    getWarehouseFile(file) match {
      case Some(warehouse) => warehouse.availableGoodNames.filter(_.startsWith(prefix)).map(name => Completion(name, name))
      case _ => Seq.empty
    }
  }

  def getWarehouseFile(file: VirtualFile)(implicit authentication: Authentication) : Option[Warehouse] =
    file.getContent.fold(_ => None, {
      case warehouse: Warehouse => Some(warehouse)
      case _ => None
    })
}

import org.enricobn.buyandsell.commands.BuyCommandArguments.*

object BuyCommand extends VirtualCommandAbstract("buy", FROM, GOOD, QTY, TO) {

  override def runParsed(shell: VirtualShell, shellInput: ShellInput, shellOutput: ShellOutput, args: Seq[Any])
                        (implicit authentication: Authentication)
  : Either[IOError, VirtualProcess] = {
      val from = args.head.asInstanceOf[VirtualFile]
      val good = args(1).asInstanceOf[String]
      val qty = args(2).asInstanceOf[Int]
      val to = args(3).asInstanceOf[VirtualFile]

      for {
        fromWarehouse <- from.contentAs(classOf[Warehouse])
        toWarehouse <- to.contentAs(classOf[Warehouse])
        gameStatsFile <- GameStatistics.get(shell)
        goodEnum <- GoodEnum.withNameE(good)
        price <- fromWarehouse.getPrice(goodEnum)
        newFromWarehouse <- fromWarehouse.change(goodEnum, -qty)
        newToWarehouse <- toWarehouse.change(goodEnum, qty)
        _ <- from.setContent(newFromWarehouse)
        _ <- gameStatsFile.mapContent(_.add(-price.base * qty))
        _ <- to.setContent(newToWarehouse)
        _ <- Messages.addMessage(shell, "buy " + qty + " of " + good)
      } yield new VirtualProcess()

  }

  def tryToEither[A](obj: Try[A]): Either[IOError, A] = {
    obj match {
      case Success(something) => Right(something)
      case Failure(err) => Left(IOError(err.getMessage))
    }
  }

}
