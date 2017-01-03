package org.enricobn.consolegame

import java.util.UUID

import org.enricobn.consolegame.commands.{MessagesCommand, SellCommand}
import org.enricobn.consolegame.content.{Messages, MessagesSerializer, Warehouse}
import org.enricobn.shell.impl._
import org.enricobn.terminal.{CanvasInputHandler, CanvasTextScreen, TerminalImpl}
import org.enricobn.vfs.impl.VirtualUsersManagerImpl
import org.enricobn.vfs.inmemory.InMemoryFS
import org.scalajs.dom
import org.scalajs.dom.FileReader
import org.scalajs.dom.html._
import org.scalajs.dom.raw._

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportAll}

// to access members of structural types (new {}) without warnings
import scala.language.reflectiveCalls

/**
  * Created by enrico on 12/8/16.
  */
@JSExport(name = "ConsoleGame")
@JSExportAll
class ConsoleGame(mainCanvasID: String, messagesCanvasID: String, loadGameID: String, saveGameID: String) {
  private val gameStateFactory = new GameStateFactory()
  gameStateFactory.register(MessagesSerializer)

  private val gameState = new GameState()
  private val mainScreen = new CanvasTextScreen(mainCanvasID)
  private val mainInput = new CanvasInputHandler(mainCanvasID)
  private val mainTerminal = new TerminalImpl(mainScreen, mainInput, "typewriter-key-1.wav")
  private val mainCanvas = dom.document.getElementById(mainCanvasID).asInstanceOf[Canvas]
  mainCanvas.contentEditable = "true"
  mainCanvas.focus()

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
    _ <- warehouseFile.chown("guest").toLeft(None).right
    _ <- (warehouseFile.content = warehouse).toLeft(None).right
    messagesFile <- log.touch("messages.log").right
    _ <- (messagesFile.content = messages).toLeft(None).right
    _ <- context.createCommandFile(bin, new LsCommand).right
    _ <- context.createCommandFile(bin, new CdCommand).right
    _ <- context.createCommandFile(bin, new CatCommand).right
    _ <- context.createCommandFile(usrBin, new SellCommand(messages)).right
    _ <- context.createCommandFile(usrBin, new MessagesCommand(messages)).right
  } yield {
    gameState.add(messagesFile)
    new {
      val currentFolder = guest
      val path = List(bin, usrBin)
    }
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
    val loadGame = dom.document.getElementById(loadGameID).asInstanceOf[Input]
    loadGame.addEventListener("change", readGame(loadGame) _, false)
    val saveGameAnchor = dom.document.getElementById(saveGameID).asInstanceOf[Anchor]
    saveGameAnchor.onclick = saveGame(saveGameAnchor) _
  }

  def saveGame(anchor: Anchor)(evt: MouseEvent): Unit = {
    gameStateFactory.save(gameState) match {
      case Left(error) => dom.window.alert(s"Error saving game: ${error.message}.")
      case Right(s) =>
        val file = new Blob(js.Array(s), BlobPropertyBag("text/plain"))
        anchor.href = URL.createObjectURL(file)
        anchor.pathname = "consolegame.json"
    }
  }

  def readGame(input: Input)(evt: Event): Unit = {
      //Retrieve the first (and only!) File from the FileList object
      val f = evt.target.asInstanceOf[Input].files(0)

      if (f != null) {
        val r = new FileReader()
        r.onload = {e: UIEvent => {
          println("loading...")
          val content = r.result.toString
          vum.logUser("root", rootPassword)
          gameStateFactory.load(content, fs) match {
            case Left(error) => dom.window.alert(s"Error loading game: ${error.message}.")
            case Right(gs) =>
              println(gs.contents)
          }
          vum.logUser("guest", guestPassword)
//          val contents = e.target.asInstanceOf[Input].result
//          dom.window.alert( "Got the file.n"
//            + "name: " + f.name + "\n"
//            + "type: " + f.`type` + "\n"
//            + "size: " + f.size + " bytes\n"
//            + "starts with: " + contents.substr(1, contents.indexOf('\n'))
//          )
        }}
        r.readAsText(f)
      }
  }

}
