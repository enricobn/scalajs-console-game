package org.enricobn.consolegame

import java.util.UUID

import org.enricobn.buyandsell.BuyAndSellGameStateFactory
import org.enricobn.consolegame.commands.{MessagesCommand, SellCommand}
import org.enricobn.consolegame.content.{Messages, Warehouse}
import org.enricobn.shell.impl._
import org.enricobn.terminal.{CanvasInputHandler, CanvasTextScreen, JSLogger, TerminalImpl}
import org.enricobn.vfs.IOError
import org.enricobn.vfs.impl.VirtualUsersManagerImpl
import org.enricobn.vfs.inmemory.InMemoryFS
import org.enricobn.vfs.utils.Utils
import org.scalajs.dom
import org.scalajs.dom.FileReader
import org.scalajs.dom.html.{Anchor, Canvas, Input}
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
  private val gameStateFactory = BuyAndSellGameStateFactory
  private var gameState = gameStateFactory.create()
  private val logger = new JSLogger()
  private val mainScreen = new CanvasTextScreen(mainCanvasID, logger)
  private val mainInput = new CanvasInputHandler(mainCanvasID)
  private val mainTerminal = new TerminalImpl(mainScreen, mainInput, logger, "typewriter-key-1.wav")
  private val mainCanvas = dom.document.getElementById(mainCanvasID).asInstanceOf[Canvas]
  mainCanvas.contentEditable = "true"
  mainCanvas.focus()

  private val messagesScreen = new CanvasTextScreen(messagesCanvasID, logger)
  private val messagesInput = new CanvasInputHandler(messagesCanvasID)
  private val messagesTerminal = new TerminalImpl(messagesScreen, messagesInput, logger, "typewriter-key-1.wav")

  private val rootPassword = UUID.randomUUID().toString
  private val vum = new VirtualUsersManagerImpl(rootPassword)

  private val guestPassword = UUID.randomUUID().toString
  vum.addUser("guest", guestPassword)

  private val fs = new InMemoryFS(vum)
  private val root = fs.root

  private val context = new VirtualShellContextImpl()
  private val shell = new VirtualShell(mainTerminal, vum, context, root)
  private val messagesShell = new VirtualShell(messagesTerminal, vum, context, root)

  def start() {
    initFS()
      .orElse(newGame())
      .orElse(initUserCommands())
      .orElse(vum.logUser("guest", guestPassword))
      .orElse({
        root.resolveFolderOrError("/home/guest", "Cannot find /home/guest") match {
          case Left(error) => Some(error)
          case Right(guestFolder) =>
            shell.currentFolder = guestFolder
            None
        }
      })
    match {
      case Some(error) => dom.window.alert(s"Error initializing: ${error.message}")
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
        initUserCommands()
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


  private def initFS(): Option[IOError] = {
    val job = for {
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

    job.left.toOption
  }

  private def newGame(): Option[IOError] = {
    val warehouse = new Warehouse()
    warehouse.add("gold", 2)
    warehouse.add("silver", 10)
    warehouse.add("bronze", 20)

    val messages = new Messages()

    val job = for {
      log <- root.resolveFolderOrError("/var/log", "Cannot find folder /var/log.").right
      guest <- root.resolveFolderOrError("/home/guest", "Cannot find folder /home/guest.").right
      warehouseFile <- guest.touch("warehouse").right
      _ <- warehouseFile.chown("guest").toLeft(None).right
      _ <- (warehouseFile.content = warehouse).toLeft(None).right
      messagesFile <- log.touch("messages.log").right
      _ <- (messagesFile.content = messages).toLeft(None).right
    } yield {
      gameState.setMessages(messagesFile, messages)
      gameState.add(warehouseFile, warehouse)
    }

    job.left.toOption

  }

  private def initUserCommands(): Option[IOError] = {
    val job = for {
      usrBin <- root.resolveFolderOrError("/usr/bin", "Cannot find folder /usr/bin.").right
      log <- root.resolveFolderOrError("/var/log", "Cannot find folder /var/log.").right
      messagesFile <- log.findFileOrError("messages.log", "Cannot find file /var/log/messages.log.").right
      content <- messagesFile.content.right
      _ <- context.createCommandFile(usrBin, new SellCommand(content.asInstanceOf[Messages])).right
      _ <- context.createCommandFile(usrBin, new MessagesCommand(content.asInstanceOf[Messages])).right
    } yield None

    job.left.toOption
  }

  private def deleteUserCommands(): Option[IOError] = {
    val job = for {
      usrBin <- root.resolveFolderOrError("/usr/bin", "Cannot find folder /usr/bin.").right
      _ <- Utils.optionToLeft(usrBin.deleteFile(SellCommand.NAME)).right
      _ <- Utils.optionToLeft(usrBin.deleteFile(MessagesCommand.NAME)).right
    } yield None

    job.left.toOption
  }

}
