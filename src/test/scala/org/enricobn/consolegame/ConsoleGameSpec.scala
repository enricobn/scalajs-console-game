package org.enricobn.consolegame

import org.enricobn.consolegame.ConsoleGameSpec.scheduler
import org.enricobn.shell.impl.{Scheduler, UnixLikeInMemoryFS, VirtualShell}
import org.enricobn.terminal.{JSLogger, Terminal}
import org.enricobn.vfs.IOError
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

object ConsoleGameSpec {
  private val scheduler = new Scheduler {
    override def run(callback: Double => Unit): Unit = {}
  }
}

class ConsoleGameSpec extends FlatSpec with MockFactory with Matchers {

  "createMainShell" should "log with root" in {
    val mainTerminal = mock[Terminal]

    val shell = ConsoleGame.createMainShell("root", mainTerminal, scheduler).right.get
    assert(shell.vum.getUser(shell.authentication).get == "root")
  }

  "createMainShell" should "create an UnixLikeInMemoryFS" in {
    val mainTerminal = mock[Terminal]

    val shell = ConsoleGame.createMainShell("root", mainTerminal, scheduler).right.get
    assert(shell.fs.isInstanceOf[UnixLikeInMemoryFS])
  }

  "onNewGame" should "create and log a user" in {
    val mainTerminal = mock[Terminal]
    val messagesTerminal = stub[Terminal]
    val logger = stub[JSLogger]

    (mainTerminal.add _).expects(*).anyNumberOfTimes()
    (mainTerminal.flush : () => Unit).expects().anyNumberOfTimes()

    (mainTerminal.removeOnInput _).expects(*).anyNumberOfTimes()

    (mainTerminal.onInput _).expects(*).onCall { subscriber: _root_.org.enricobn.terminal.StringPub#Sub =>
      subscriber.notify(null, "enrico")
      subscriber.notify(null, Terminal.CR)
    }.anyNumberOfTimes()

    val game = new ConsoleGame(mainTerminal, messagesTerminal, logger, scheduler) {

      override def onNewGame(shell: VirtualShell): Either[IOError, Unit] = Right(())

      override def commands: Either[IOError, Seq[GameCommand]] = Right(List())

      override def getSerializers: Seq[Serializer] = List()

      override def getBackgroundCommand: Option[(String, List[String])] = None

      override def showError(message: String): Unit = fail(message)

      override def executeLater(runnable: () => Unit): Unit = {
        runnable.apply()
      }

      override def saveToFile(content: String, fileName: String): Unit = {}

    }

    game.onNewGame()

    assert("enrico" == game.shell.vum.getUser(game.shell.authentication).get)

    assert("/home/enrico" == game.shell.currentFolder.path)
  }

}
