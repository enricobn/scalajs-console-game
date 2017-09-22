package org.enricobn.consolegame

import java.util.UUID

import org.enricobn.consolegame.commands.MessagesCommand
import org.enricobn.shell.VirtualShellContext
import org.enricobn.shell.impl._
import org.enricobn.terminal.{CanvasInputHandler, CanvasTextScreen, JSLogger, TerminalImpl}
import org.enricobn.vfs.impl.VirtualUsersManagerImpl
import org.enricobn.vfs.inmemory.InMemoryFS
import org.enricobn.vfs.utils.Utils
import org.enricobn.vfs.{IOError, VirtualFile, VirtualFolder}
import org.scalajs.dom
import org.scalajs.dom.FileReader
import org.scalajs.dom.html.{Anchor, Canvas, Input}
import org.scalajs.dom.raw._

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportAll

// to access members of structural types (new {}) without warnings
import scala.language.reflectiveCalls

/**
  * Created by enrico on 12/8/16.
  */
@JSExportAll
abstract class ConsoleGame[GS <: GameState[GSS], GSS <: AnyRef, GSF <: GameStateFactory[GSS, GS]]
    (mainCanvasID: String, messagesCanvasID: String, loadGameID: String, saveGameID: String, gameStateFactory: GSF) {
  private var gameState = gameStateFactory.create()
  private val logger = new JSLogger()
  private val mainScreen = new CanvasTextScreen(mainCanvasID, logger)
  private val mainInput = new CanvasInputHandler(mainCanvasID)
  private val mainTerminal = new TerminalImpl(mainScreen, mainInput, logger, "typewriter-key-1.wav")
  private val mainCanvas = dom.document.getElementById(mainCanvasID).asInstanceOf[Canvas]
  private val messagesScreen = new CanvasTextScreen(messagesCanvasID, logger)
  private val messagesInput = new CanvasInputHandler(messagesCanvasID)
  private val messagesTerminal = new TerminalImpl(messagesScreen, messagesInput, logger, "typewriter-key-1.wav")
  private val rootPassword = UUID.randomUUID().toString
  private val vum = new VirtualUsersManagerImpl(rootPassword)
  private val guestPassword = UUID.randomUUID().toString
  private val fs = new InMemoryFS(vum)
  protected val context: VirtualShellContext = new VirtualShellContextImpl()
  protected val root: VirtualFolder = fs.root
  private val shell = new VirtualShell(mainTerminal, vum, context, root)
  private val messagesShell = new VirtualShell(messagesTerminal, vum, context, root)

  private var userCommands : Seq[VirtualFile] = _

  def start() {
    mainCanvas.contentEditable = "true"
    mainCanvas.focus()

    val init = for {
      _ <- vum.addUser("guest", guestPassword).toLeft(()).right
      _ <- initFS().right
      _ <- newGame(gameState).toLeft(()).right
      userCommands <- createUserCommands().right
      guestFolder <- root.resolveFolderOrError("/home/guest", "Cannot find /home/guest").right
      _ <- vum.logUser("guest", guestPassword).toLeft(()).right
    } yield {
      this.userCommands = userCommands
      shell.currentFolder = guestFolder
    }

    init match {
      case Left(error) => dom.window.alert(s"Error initializing: ${error.message}")
      case _ =>
        shell.start()
        messagesShell.startWithCommand(MessagesCommand.NAME)
        val loadGame = dom.document.getElementById(loadGameID).asInstanceOf[Input]
        loadGame.addEventListener("change", readGame(loadGame) _, false)
        val saveGameAnchor = dom.document.getElementById(saveGameID).asInstanceOf[Anchor]
        saveGameAnchor.onclick = saveGame(saveGameAnchor) _
    }
  }

  private def saveGame(anchor: Anchor)(evt: MouseEvent): Unit = {
    gameStateFactory.serialize(gameState) match {
      case Left(error) => dom.window.alert(s"Error saving game: ${error.message}.")
      case Right(s) =>
        val file = new Blob(js.Array(s), BlobPropertyBag("text/plain"))
        anchor.href = URL.createObjectURL(file)
        anchor.pathname = "consolegame.json"
        messagesTerminal.add(s"Game saved.\n")
        messagesTerminal.flush()
    }
  }

  private def readGame(input: Input)(evt: Event): Unit = {
      //Retrieve the first (and only!) File from the FileList object
      val f = evt.target.asInstanceOf[Input].files(0)

      if (f != null) {
        val r = new FileReader()
        r.onload = fileReaderOnLoad(f, r) _
        r.readAsText(f)
      }
  }

  private def fileReaderOnLoad(f: File, r: FileReader)(e: UIEvent) {
      println("loading...")
      val content = r.result.toString
      vum.logUser("root", rootPassword)
      messagesShell.stopInteractiveCommands({ () =>
        gameState.delete()
        //            gameState.contents.foreach(file => {
        //              file.parent.deleteFile(file.name)
        //            }
        deleteUserCommands()
        gameStateFactory.deserialize(content, fs) match {
          case Left(error) => dom.window.alert(s"Error loading game: ${error.message}.")
          case Right(gs) =>
            gameState = gs
        }
        messagesShell.run(MessagesCommand.NAME) match {
          case Left(error) => dom.window.alert(s"Error restaring messages: ${error.message}.")
          case _ =>
        }
        vum.logUser("guest", guestPassword)
        messagesTerminal.add(s"Game loaded from ${f.name}\n")
        messagesTerminal.flush()
        false
      })
  }

  private def initFS(): Either[IOError,Unit] =
    for {
      bin <- root.mkdir("bin").right
      usr <- root.mkdir("usr").right
      var_ <- root.mkdir("var").right
      log <- var_.mkdir("log").right
      usrBin <- usr.mkdir("bin").right
      home <- root.mkdir("home").right
      guest <- home.mkdir("guest").right
      _ <- context.createCommandFile(bin, new LsCommand).right
      _ <- context.createCommandFile(bin, new CdCommand).right
      _ <- context.createCommandFile(bin, new CatCommand).right
    } yield {
      context.addToPath(bin)
      context.addToPath(usrBin)
    }

  def newGame(gameState: GS): Option[IOError]

  def createUserCommands(): Either[IOError,Seq[VirtualFile]]

  private def deleteUserCommands(): Option[IOError] = Utils.mapFirstSome(userCommands, GameState.delete)

}
