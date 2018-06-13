package org.enricobn.consolegame

import java.util.UUID

import org.enricobn.consolegame.commands.MessagesCommand
import org.enricobn.consolegame.content.{Messages, MessagesSerializer}
import org.enricobn.shell.impl._
import org.enricobn.shell.{VirtualCommand, VirtualShellContext}
import org.enricobn.terminal._
import org.enricobn.vfs._
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

object ConsoleGame {
  // TODO move in VirtualFile?
  def delete(file: VirtualFile)(implicit authentication: Authentication) : Option[IOError] =
    file.parent match {
      case Some(folder) => folder.deleteFile(file.name)
      case _ => Some(IOError("No parent"))
    }

  def initFS(fs: VirtualFS, vum: VirtualUsersManager, context: VirtualShellContext, userName: String,
             allCommands : Either[IOError, Seq[VirtualCommand]])(implicit authentication: Authentication) : Either[IOError,Unit] =
    for {
      bin <- mkdir(fs.root, "bin").right
      usr <- mkdir(fs.root, "usr").right
      var_ <- mkdir(fs.root, "var").right
      varLog <- mkdir(var_, "log").right
      usrBin <- mkdir(usr, "bin").right
      home <- mkdir(fs.root, "home").right
      // TODO would it be better if I create the home folder in vum.addUser?
      userHome <- mkdir(home, userName).right
      _ <- userHome.chown(userName).toLeft(()).right
      _ <- context.createCommandFile(bin, new LsCommand).right
      _ <- context.createCommandFile(bin, new CdCommand).right
      _ <- context.createCommandFile(bin, new CatCommand).right
      userCommands <- allCommands.right
      _ <- Utils.lift(userCommands.map(command => {
        context.createCommandFile(usrBin, command)
      })).right
    } yield {
      for {
        messagesLog <- varLog.findFile("messages.log").right
        messagesFile <- (if (messagesLog.isDefined) { Right(messagesLog.get) } else { varLog.createFile("messages.log", Messages(Seq.empty)) }).right
        // TODO I don't like that user has the ability to delete the file, remove logs etc...
        // But it's not easy to avoid that and grant access to add messages, perhaps I need some
        // system call handling, but I think it's too complicated for this project ...
        _ <- messagesFile.chown(userName).toLeft(()).right
      } yield {
        context.addToPath(bin)
        context.addToPath(usrBin)
      }
    }

  private def mkdir(parentFolder: VirtualFolder, name: String)(implicit authentication: Authentication) : Either[IOError, VirtualFolder] = {
    parentFolder.findFolder(name) match {
      case Right(Some(folder)) => Right(folder)
      case Right(None) => parentFolder.mkdir(name)
      case Left(error) => Left(error)
    }
  }
}

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
  private val userPassword = UUID.randomUUID().toString
  private var fs = new InMemoryFS(rootPassword)
  private var vum = fs.vum
  private var vsm = fs.vsm
  private var context: VirtualShellContext = new VirtualShellContextImpl()
  private var rootAuthentication = vum.logRoot(rootPassword).right.get
  private var shell = new VirtualShell(mainTerminal, vum, vsm, context, fs.root, rootAuthentication)
  private var messagesShell = new VirtualShell(messagesTerminal, vum, vsm, context, fs.root, rootAuthentication)

  private var userName : String = _

  def start() {
    mainCanvas.contentEditable = "true"
    mainCanvas.focus()

    mainTerminal.add("Your name: ")
    mainTerminal.flush()

    shell.readLine({ s =>
      userName = s
      runInit()
    })

  }

  private def runInit() {
    val runInit = for {
      _ <- vum.addUser(userName, userPassword)(rootAuthentication).toLeft(()).right
      _ <- ConsoleGame.initFS(fs, vum, context, userName, allCommands)(rootAuthentication).right
      userHome <- shell.toFolder("/home/" + userName).right
      _ <- shell.login(userName, userPassword).right
      _ <- onNewGame(shell).toLeft(()).right
    } yield {
      shell.currentFolder = userHome
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
    val job =
      for {
        serializedFS <- SerializedFSOperations.build(allFiles, getAllFolders(fs.root), getSerializersMap)(rootAuthentication).right
        ser <- UpickleUtils.writeE(serializedFS).right
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

  protected def getSerializersMap: Map[String, Serializer] = {
    val serializers: Map[String, Serializer] = (getSerializers ++ getGlobalSerializers).map(serializer =>
      (serializer.clazz.getName, serializer)
    ).toMap
    serializers
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

    val newFs = new InMemoryFS(rootPassword)

    val newRootAuthentication = newFs.vum.logRoot(rootPassword).right.get

    val newContext = new VirtualShellContextImpl()

    val newShell = new VirtualShell(mainTerminal, newFs.vum, newFs.vsm, newContext, newFs.root, newRootAuthentication)

    // TODO is stopInteractiveCommands needed?
    messagesShell.stopInteractiveCommands({ () =>
      implicit val authentication : Authentication = newRootAuthentication

      val run = for {
        _ <- newFs.vum.addUser(userName, userPassword).toLeft(()).right
        serializedFS <- UpickleUtils.readE[SerializedFS](resultContent).right
        _ <- SerializedFSOperations.load(newShell, getSerializersMap, serializedFS).right
        showPrompt <- messagesShell.run(MessagesCommand.NAME).right
        _ <- ConsoleGame.initFS(newFs, newFs.vum, newContext, userName, allCommands).right
        _ <- newShell.login(userName, userPassword).right
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
          vum = newFs.vum
          vsm = newFs.vsm
          context = newContext
          rootAuthentication = newRootAuthentication

          messagesShell.stop()
          messagesShell = new VirtualShell(messagesTerminal, vum, vsm, context, fs.root, newRootAuthentication)
          messagesShell.startWithCommand(MessagesCommand.NAME)

          shell.stop()
          shell = newShell
          // TODO error
          shell.currentFolder = shell.toFolder("/home/" + userName).right.get
          mainTerminal.add("\u001b[2J\u001b[1;1H") // clear screen an reset cursor to 1, 1
          shell.start()

          showPrompt
      }

    })

    // TODO Error
    vum.logUser(userName, userPassword)
  }

  def onNewGame(shell: VirtualShell): Option[IOError]

  def commands : Either[IOError, Seq[VirtualCommand]]

  private def allCommands : Either[IOError, Seq[VirtualCommand]] = {
    for {
      defCommands <- Right(defaultCommands).right
      commands <- commands.right
    } yield defCommands ++ commands
  }

  private def defaultCommands : Seq[VirtualCommand] = Seq(new MessagesCommand())

  def getSerializers: Seq[Serializer]

  // TODO error for logging (even for getAllFiles?)
  private def allFiles: Set[VirtualFile] = {
    implicit val authentication : Authentication = rootAuthentication

    vum.logRoot(rootPassword)
    val files = getAllFiles(fs.root)
    vum.logUser(userName, userPassword)
    files
  }

  // TODO create scalajs-vfs VirtualFolder.getAllFiles
  // TODO error?
  private def getAllFiles(folder: VirtualFolder) : Set[VirtualFile] = {
    implicit val authentication : Authentication = rootAuthentication

    (for {
      files <- folder.files.right
      folders <- folder.folders.right
    } yield {
      files ++ folders.flatMap(getAllFiles)
    }) match {
      case Left(error) => Set()
      case Right(files) => files
    }
  }

  // TODO create scalajs-vfs VirtualFolder.getAllFolders
  // TODO error?
  private def getAllFolders(folder: VirtualFolder) : List[VirtualFolder] = {
    implicit val authentication : Authentication = rootAuthentication

    (for {
      folders <- folder.folders.right
    } yield {
      folders.toList ++ folders.flatMap(getAllFolders)
    }) match {
      case Left(error) => List()
      case Right(folders) => folders
    }
  }


  private def getFile(path: String): Either[IOError, VirtualFile] = {
    shell.toFile(path)
  }

}

case class SerializedFile(path: String, owner: String, permissions: Int, serializerName: String, ser: String)

case class SerializedFolder(path: String, owner: String, permissions: Int)

case class SerializedFS(folders: List[SerializedFolder], files: List[SerializedFile])