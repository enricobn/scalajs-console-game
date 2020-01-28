package org.enricobn.consolegame

import org.enricobn.shell.impl.{UnixLikeInMemoryFS, VirtualShell}
import org.enricobn.terminal.{JSLogger, Terminal}
import org.enricobn.vfs.IOError
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

class ConsoleGameSpec extends FlatSpec with MockFactory with Matchers {

  "createMainShell" should "log with root" in {
    val term = mock[Terminal]

    val shell = ConsoleGame.createMainShell("root", term).right.get
    assert(shell.vum.getUser(shell.authentication).get == "root")
  }

  "createMainShell" should "create an UnixLikeInMemoryFS" in {
    val term = mock[Terminal]

    val shell = ConsoleGame.createMainShell("root", term).right.get
    assert(shell.fs.isInstanceOf[UnixLikeInMemoryFS])
  }

  "onNewGame" should "work" in {
    val mainTerminal =  mock[Terminal]
    val messagesTerminal =  mock[Terminal]
    val logger = mock[JSLogger]

    val game = new ConsoleGame(mainTerminal, messagesTerminal, logger) {
      override def onNewGame(shell: VirtualShell): Either[IOError, Unit] = Right(())

      override def commands: Either[IOError, Seq[GameCommand]] = Right(List())

      override def getSerializers: Seq[Serializer] = List()

      override def getBackgroundCommand: Option[(String, List[String])] = None

      override def showError(message: String): Unit = fail(message)

      override def executeLater(runnable: () => Unit): Unit = runnable()

    }
    //game.onNewGame()
  }

}
