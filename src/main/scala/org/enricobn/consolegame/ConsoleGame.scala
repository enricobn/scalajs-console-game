package org.enricobn.consolegame

import java.util.UUID

import org.enricobn.shell.impl.{CatCommand, CdCommand, LsCommand, VirtualShell}
import org.enricobn.terminal.{CanvasInputHandler, CanvasTextScreen, TerminalImpl}
import org.enricobn.vfs.impl.VirtualUsersManagerImpl
import org.enricobn.vfs.inmemory.InMemoryFS

import scala.scalajs.js.annotation.{JSExport, JSExportAll}

/**
  * Created by enrico on 12/8/16.
  */
@JSExport(name = "ConsoleGame")
@JSExportAll
class ConsoleGame(canvasid: String) {
  private val screen = new CanvasTextScreen(canvasid)
  private val input = new CanvasInputHandler(canvasid)
  private val terminal = new TerminalImpl(screen, input, "typewriter-key-1.wav")
  private val rootPassword = UUID.randomUUID().toString
  private val vum = new VirtualUsersManagerImpl(rootPassword)

  private val guestPassword = UUID.randomUUID().toString
  vum.addUser("guest", guestPassword)

  private val fs = new InMemoryFS(vum)
  private val root = fs.root
  private val goods = new Goods
  goods.add("gold", 2)
  goods.add("silver", 10)
  goods.add("bronze", 20)

  private val shell = new VirtualShell(terminal, vum, root)

  private val job = for {
    bin <- root.mkdir("bin").right
    usr <- root.mkdir("usr").right
    usrBin <- usr.mkdir("bin").right
    home <- root.mkdir("home").right
    guest <- home.mkdir("guest").right
    goodsFile <- guest.touch("goods").right
    _ <- (goodsFile.content = goods).right
    _ <- shell.createCommandFile(bin, new LsCommand).right
    _ <- shell.createCommandFile(bin, new CdCommand).right
    _ <- shell.createCommandFile(bin, new CatCommand).right
    _ <- shell.createCommandFile(usrBin, new SellCommand).right
  } yield new {
    val currentFolder = guest
    val path = List(bin, usrBin)
  }

  job match {
    case Left(error) =>
      terminal.add(error.message + VirtualShell.CRLF)
      terminal.flush()
    case Right(j) =>
      j.path.foreach(shell.addToPath(_))
      shell.currentFolder = j.currentFolder
  }

  vum.logUser("guest", guestPassword)

  def start() {
    shell.start()
  }
}
