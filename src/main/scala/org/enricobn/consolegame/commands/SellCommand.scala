package org.enricobn.consolegame.commands

import org.enricobn.consolegame.content.{Goods, Messages}
import org.enricobn.shell._
import org.enricobn.shell.impl._
import org.enricobn.vfs.IOError._
import org.enricobn.vfs._

/**
  * Created by enrico on 12/17/16.
  */
class SellCommand(private val messages: Messages) extends VirtualCommand {
  private val arguments = new VirtualCommandArguments(List(
    new FileArgument("goodsFile", true) {
      override def complete(currentFolder: VirtualFolder, value: String, previousArguments: Seq[Any]): Seq[String] = {
        fileNameProposals(currentFolder, value)
      }
    },
    new StringArgument("good", true) {
      override def complete(currentFolder: VirtualFolder, value: String, previousArguments: Seq[Any]): Seq[String] = {
        goodsProposals(previousArguments.head.asInstanceOf[VirtualFile], value)
      }
    },
    new IntArgument("qty", true)
  ))

  override def getName: String = "sell"

  override def run(shell: VirtualShell, shellInput: ShellInput, shellOutput: ShellOutput, args: String*) = {
    arguments.parse(shell.currentFolder, args: _*) match {
      case Left(message) => Left(new IOError("sell: " + message))
      case Right(values) => for {
        content <- values.head.asInstanceOf[VirtualFile].content.right
        result <- content match {
          case goods: Goods =>
            val good = values(1).asInstanceOf[String]
            val qty = values(2).asInstanceOf[Int]
            goods
              .sell(good, qty)
              .toLeft({
                messages.add("sell " + qty + " of " + good)
                new RunContext()
              })
              .left
              .map(new IOError(_))
              .right
          case _ => "Not a Goods file.\n".ioErrorE.right
        }
      } yield result
    }
  }

  override def completion(line: String, currentFolder: VirtualFolder): Seq[String] = {
    arguments.complete(currentFolder, line)
  }

  private def goodsProposals(file: VirtualFile, prefix: String) : Seq[String] = {
    getGoodsFile(file) match {
      case Some(goods) => goods.goods.keySet.filter(_.startsWith(prefix)).toSeq
      case _ => Seq.empty
    }
  }

  // TODO relativeFile
  private def fileNameProposals(currentFolder: VirtualFolder, prefix: String) : Seq[String] = {
    Completions.resolveFiles(currentFolder, prefix, getGoodsFile(_).isDefined)
  }

  private def getGoodsFile(file: VirtualFile) : Option[Goods] =
    file.content.fold(_ => None, {
      case goods: Goods => Some(goods)
      case _ => None
    })

}
