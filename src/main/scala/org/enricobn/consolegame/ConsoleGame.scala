package org.enricobn.consolegame

import java.util.UUID

import org.enricobn.consolegame.commands.{MessagesCommand, SellCommand}
import org.enricobn.consolegame.content.{Messages, Warehouse}
import org.enricobn.shell.impl._
import org.enricobn.terminal.{CanvasInputHandler, CanvasTextScreen, TerminalImpl}
import org.enricobn.vfs.impl.VirtualUsersManagerImpl
import org.enricobn.vfs.inmemory.InMemoryFS

import scala.scalajs.js.annotation.{JSExport, JSExportAll}

/**
  * Created by enrico on 12/8/16.
  */
@JSExport(name = "ConsoleGame")
@JSExportAll
class ConsoleGame(mainCanvasID: String, messagesCanvasID: String) {
  private val mainScreen = new CanvasTextScreen(mainCanvasID)
  private val mainInput = new CanvasInputHandler(mainCanvasID)
  private val mainTerminal = new TerminalImpl(mainScreen, mainInput, "typewriter-key-1.wav")

  private val messagesScreen = new CanvasTextScreen(messagesCanvasID)
  private val messagesInput = new CanvasInputHandler(messagesCanvasID)
  private val messagesTerminal = new TerminalImpl(messagesScreen, messagesInput, "typewriter-key-1.wav")

  private val rootPassword = UUID.randomUUID().toString
  private val vum = new VirtualUsersManagerImpl(rootPassword)

  private val guestPassword = UUID.randomUUID().toString
  vum.addUser("guest", guestPassword)

  private val fs = new InMemoryFS(vum)
  private val root = fs.root
  private val warehouse = new Warehouse()
  warehouse.add("gold", 2)
  warehouse.add("silver", 10)
  warehouse.add("bronze", 20)

  private val context = new VirtualShellContextImpl()
  private val shell = new VirtualShell(mainTerminal, vum, context, root)
  private val messagesShell = new VirtualShell(messagesTerminal, vum, context, root)
  private val messages = new Messages()

  private val job = for {
    bin <- root.mkdir("bin").right
    usr <- root.mkdir("usr").right
    var_ <- root.mkdir("var").right
    log <- var_.mkdir("log").right
    usrBin <- usr.mkdir("bin").right
    home <- root.mkdir("home").right
    guest <- home.mkdir("guest").right
    warehouseFile <- guest.touch("warehouse").right
    _ <- Right({warehouseFile.chown("guest")}).right
    _ <- (warehouseFile.content = warehouse).right
    messagesFile <- log.touch("messages.log").right
    _ <- (messagesFile.content = messages).right
    _ <- context.createCommandFile(bin, new LsCommand).right
    _ <- context.createCommandFile(bin, new CdCommand).right
    _ <- context.createCommandFile(bin, new CatCommand).right
    _ <- context.createCommandFile(usrBin, new SellCommand(messages)).right
    _ <- context.createCommandFile(usrBin, new MessagesCommand(messages)).right
  } yield new {
    val currentFolder = guest
    val path = List(bin, usrBin)
  }

  job match {
    case Left(error) =>
      mainTerminal.add(error.message + VirtualShell.CRLF)
      mainTerminal.flush()
    case Right(j) =>
      j.path.foreach(context.addToPath(_))
      shell.currentFolder = j.currentFolder
  }

  vum.logUser("guest", guestPassword)

  def start() {
    shell.start()
    messagesShell.startWithCommand("messages")
  }

}
