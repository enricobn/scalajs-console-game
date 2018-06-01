package org.enricobn.consolegame

import java.util.UUID

import org.enricobn.consolegame.commands.MessagesCommand
import org.enricobn.consolegame.content.{Messages, MessagesSerializer}
import org.enricobn.shell.impl._
import org.enricobn.shell.{VirtualCommand, VirtualShellContext}
import org.enricobn.terminal._
import org.enricobn.vfs._
import org.enricobn.vfs.impl.VirtualUsersManagerImpl
import org.enricobn.vfs.inmemory.InMemoryFS
import org.enricobn.vfs.utils.Utils
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
abstract class ConsoleGame(mainCanvasID: String, messagesCanvasID: String, loadGameID: String, saveGameID: String) {
  private val logger = new JSLoggerImpl()
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
  private var fs = new InMemoryFS(vum)
  private val context: VirtualShellContext = new VirtualShellContextImpl()
  private var shell = new VirtualShell(mainTerminal, vum, context, fs.root)
  private var messagesShell = new VirtualShell(messagesTerminal, vum, context, fs.root)

  private var userCommands : Seq[VirtualFile] = _

  def start() {
    mainCanvas.contentEditable = "true"
    mainCanvas.focus()

    val runInit = for {
      _ <- vum.addUser("guest", guestPassword).toLeft(()).right
      _ <- initFS().right
      usrBin <- shell.toFolder("/usr/bin").right
      userCommands <- allCommands.right
      userCommandFiles <- Utils.lift(userCommands.map(command => {
        context.createCommandFile(usrBin, command)
      })).right
      guestFolder <- shell.toFolder("/home/guest").right
      _ <-vum.logUser("guest", guestPassword).toLeft(()).right
      _ <- onNewGame(shell).toLeft(()).right
    } yield {
      this.userCommands = userCommandFiles
      shell.currentFolder = guestFolder
    }

    runInit match {
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
    val serializers: Map[Class[_], Serializer] = (getSerializers ++ getGlobalSerializers).map(serializer =>
      (serializer.clazz, serializer)
    ).toMap

    val job =
      for {
        ser <- FSSerializer.save(allFiles, getAllFolders(fs.root), serializers).right
    } yield {
      val file = new Blob(js.Array(ser), BlobPropertyBag("text/plain"))
      anchor.href = URL.createObjectURL(file)
      anchor.pathname = "consolegame.json"
      messagesTerminal.add(s"Game saved.\n")
      messagesTerminal.flush()
    }

    job match {
      case Left(error) => dom.window.alert(s"Error saving game: ${error.message}.")
      case _ =>
    }
  }

  private def getGlobalSerializers = {
    List(MessagesSerializer)
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
    val resultContent = r.result.toString
    vum.logUser("root", rootPassword)

    val serializers: Map[String, Serializer] = (getSerializers ++ getGlobalSerializers).map(serializer =>
      (serializer.name, serializer)
    ).toMap

    val newFs = new InMemoryFS(vum)

    val newShell = new VirtualShell(mainTerminal, vum, context, newFs.root)

    val deserialize = FSSerializer.load(newShell, serializers, resultContent)

    // TODO is stopInteractiveCommands needed?
    messagesShell.stopInteractiveCommands({ () =>
      val run = for {
        _ <- deserialize.right
        showPrompt <- messagesShell.run(MessagesCommand.NAME).right
      } yield {
        messagesTerminal.add(s"Game loaded from ${f.name}\n")
        messagesTerminal.flush()

        showPrompt
      }

      run match {
        case Left(error) =>
          println(error.message)
          dom.window.alert("Error loading game. See javascript console for details.")
          false
        case Right(showPrompt) =>
          fs = newFs

          messagesShell.stop()
          messagesShell = new VirtualShell(messagesTerminal, vum, context, fs.root)
          messagesShell.startWithCommand(MessagesCommand.NAME)

          shell.stop()
          shell = newShell
          // TODO error
          shell.currentFolder = shell.toFolder("/home/guest").right.get
          mainTerminal.add("\u001b[2J\u001b[1;1H") // clear screen an reset cursor to 1, 1
          shell.start()

          showPrompt
      }

    })

    // TODO Error
    vum.logUser("guest", guestPassword)
  }

  private def initFS(): Either[IOError,Unit] =
    for {
      bin <- fs.root.mkdir("bin").right
      usr <- fs.root.mkdir("usr").right
      var_ <- fs.root.mkdir("var").right
      log <- var_.mkdir("log").right
      usrBin <- usr.mkdir("bin").right
      home <- fs.root.mkdir("home").right
      // TODO would it be better if I create the home folder in vum.addUser?
      guest <- home.mkdir("guest").right
      _ <- guest.chown("guest").toLeft(()).right
      _ <- context.createCommandFile(bin, new LsCommand).right
      _ <- context.createCommandFile(bin, new CdCommand).right
      _ <- context.createCommandFile(bin, new CatCommand).right
    } yield {
      context.addToPath(bin)
      context.addToPath(usrBin)
    }

  def onNewGame(shell: VirtualShell): Option[IOError]

  def commands: Either[IOError, Seq[VirtualCommand]]

  private def allCommands : Either[IOError, Seq[VirtualCommand]] = {
    for {
      defCommands <- defaultCommands.right
      commands <- commands.right
    } yield defCommands ++ commands
  }

  private def defaultCommands: Either[IOError, Seq[VirtualCommand]] = {
    val messages = Messages(Seq.empty)

    for {
      log <- shell.toFolder("/var/log").right
      messagesFile <- log.createFile("messages.log", messages).right
      // TODO I don't like that guest has the ability to delete the file, remove logs etc...
      // But it's not easy to avoid that and grant access to add messages, perhaps I need some
      // system call handling, but I think it's too complicated for this project ...
      _ <- messagesFile.chown("guest").toLeft(()).right
    } yield Seq(new MessagesCommand())
  }

  def getSerializers: Seq[Serializer]

  private def deleteUserCommands(): Option[IOError] = Utils.mapFirstSome(userCommands, ConsoleGame.delete)

  // TODO error
  private def allFiles: Set[VirtualFile] = {
    vum.logRoot(rootPassword)
    val files = getAllFiles(fs.root)
    vum.logUser("guest", guestPassword)
    files
  }

  // TODO create scalajs-vfs VirtualFolder.getAllFiles
  // TODO error
  private def getAllFiles(folder: VirtualFolder) : Set[VirtualFile] =
    (for {
      files <- folder.files.right
      folders <- folder.folders.right
    } yield {
      files ++ folders.flatMap(getAllFiles)
    }) match {
      case Left(error) => Set()
      case Right(files) => files
    }

  // TODO create scalajs-vfs VirtualFolder.getAllFolders
  // TODO error
  private def getAllFolders(folder: VirtualFolder) : List[VirtualFolder] =
    (for {
      folders <- folder.folders.right
    } yield {
      folders.toList ++ folders.flatMap(getAllFolders)
    }) match {
      case Left(error) => List()
      case Right(folders) => folders
    }


  private def getFile(path: String): Either[IOError, VirtualFile] = {
    shell.toFile(path)
  }

}

object ConsoleGame {
  // TODO move in VirtualFile?
  def delete(file: VirtualFile): Option[IOError] =
    file.parent match {
      case Some(folder) => folder.deleteFile(file.name)
      case _ => Some(IOError("No parent"))
    }

}

private case class SerializedFile(path: String, owner: String, permissions: Int, serializerName: String, ser: String)
private case class SerializedFolder(path: String, owner: String, permissions: Int)
private case class SerializedFS(folders: List[SerializedFolder], files: List[SerializedFile])