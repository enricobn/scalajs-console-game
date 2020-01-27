package org.enricobn.consolegame

import org.enricobn.shell.impl.UnixLikeInMemoryFS
import org.enricobn.terminal.Terminal
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

}
