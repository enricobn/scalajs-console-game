package org.enricobn.consolegame

import java.util.UUID

import org.enricobn.consolegame.commands.MessagesCommand
import org.enricobn.consolegame.content.{Messages, MessagesSerializer, SimpleSerializer}
import org.enricobn.shell.impl._
import org.enricobn.shell.{VirtualCommand, VirtualShellContext}
import org.enricobn.terminal._
import org.enricobn.vfs._
import org.enricobn.vfs.impl.UnixLikeInMemoryFS
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

  def initFS(fs: UnixLikeInMemoryFS, vum: VirtualUsersManager, context: VirtualShellContext, userName: String,
             allCommands : Either[IOError, Seq[VirtualCommand]])(implicit authentication: Authentication) : Either[IOError,Unit] =
    for {
      _ <- context.createCommandFiles(fs.bin, new LsCommand, new CdCommand, new CatCommand).right
      userCommands <- allCommands.right
      _ <- context.createCommandFiles(fs.usrBin, userCommands :_*).right
      messagesLog <- fs.varLog.findFile("messages.log").right
      messagesFile <- (if (messagesLog.isDefined) { Right(messagesLog.get) } else { fs.varLog.createFile("messages.log", Messages(Seq.empty)) }).right
      // TODO I don't like that user has the ability to delete the file, remove logs etc...
      // But it's not easy to avoid that and grant access to add messages, perhaps I need some
      // system call handling, but I think it's too complicated for this project ...
      _ <- messagesFile.chown(userName).toLeft(()).right
    } yield ()

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
  private var fs : UnixLikeInMemoryFS = _
  private var vum : VirtualUsersManager = _
  private var context: VirtualShellContext = _
  private var rootAuthentication: Authentication = _
  private var shell: VirtualShell = _
  private var messagesShell: VirtualShell = _

  UnixLikeInMemoryFS(rootPassword) match {
    case Right(unixLikeInMemoryFS) =>
      fs = unixLikeInMemoryFS
      vum = fs.vum
      rootAuthentication = vum.logRoot(rootPassword).right.get
      shell = UnixLikeVirtualShell(fs, mainTerminal, fs.root, rootAuthentication)
      context = shell.context
      messagesShell = UnixLikeVirtualShell(fs, messagesTerminal, fs.root, rootAuthentication)
    case Left(error) => dom.window.alert(s"Error initializing: ${error.message}")
  }

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
    implicit val authentication: Authentication = rootAuthentication

    val runInit = for {
      _ <- vum.addUser(userName, userPassword).toLeft(()).right
      _ <- ConsoleGame.initFS(fs, vum, context, userName, allCommands).right
      userHome <- shell.toFolder("/home/" + userName).right
      _ <- messagesShell.login(userName, userPassword).right
      _ <- shell.login(userName, userPassword).right
      bin <- fs.root.resolveFolderOrError("/bin").right
      usrBin <- fs.root.resolveFolderOrError("/usr/bin").right
      _ <- context.addToPath(bin).right
      _ <- context.addToPath(usrBin).right
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
    List(MessagesSerializer, StringListSerializer, StringMapSerializer)
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

    // TODO error handling
    val newFs = UnixLikeInMemoryFS(rootPassword).right.get

    val newRootAuthentication = newFs.vum.logRoot(rootPassword).right.get

    val newShell = UnixLikeVirtualShell(newFs, mainTerminal, newFs.root, newRootAuthentication)

    // TODO is stopInteractiveCommands needed?
    messagesShell.stopInteractiveCommands({ () =>
      implicit val authentication : Authentication = newRootAuthentication

      val run = for {
        _ <- newFs.vum.addUser(userName, userPassword).toLeft(()).right
        serializedFS <- UpickleUtils.readE[SerializedFS](resultContent).right
        _ <- SerializedFSOperations.load(newShell, getSerializersMap, serializedFS).right
        showPrompt <- messagesShell.run(MessagesCommand.NAME).right
        _ <- ConsoleGame.initFS(newFs, newFs.vum, newShell.context, userName, allCommands).right
        _ <- newShell.login(userName, userPassword).right
      } yield {
        showPrompt
      }

      run match {
        case Left(error) =>
          println(error.message)
          dom.window.alert("Error loading game. See javascript console for details.")
          false
        case Right(showPrompt) =>
          try {
            messagesTerminal.add(s"Game loaded from ${f.name}\n")
            messagesTerminal.flush()

            fs = newFs
            vum = newFs.vum
            context = newShell.context
            rootAuthentication = newRootAuthentication

            println("1")

            // TODO error
            val homeFolder = newShell.toFolder("/home/" + userName).right.get

            messagesShell.stop()
            messagesShell = UnixLikeVirtualShell(newFs, messagesTerminal, homeFolder, newRootAuthentication)

            // TODO Error
            messagesShell.login(userName, userPassword)

            messagesShell.startWithCommand(MessagesCommand.NAME)

            shell.stop()
            shell = newShell
            shell.currentFolder = homeFolder
            mainTerminal.add("\u001b[2J\u001b[1;1H") // clear screen an reset cursor to 1, 1
            shell.start()
          } catch {
            case e: Exception =>
              e.printStackTrace()

              dom.window.alert("Error loading game. See javascript console for details.")

              throw e
          }

          showPrompt
      }

    })

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
      case Left(_) => Set()
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
      case Left(_) => List()
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

object StringListSerializer extends SimpleSerializer(classOf[StringList]) {

  override def serializeIt(content: StringList): Either[IOError, String] = UpickleUtils.writeE(content)

  override def deserialize(ser: String): Either[IOError, AnyRef] = UpickleUtils.readE[StringList](ser)

}

object StringMapSerializer extends SimpleSerializer(classOf[StringMap]) {

  override def serializeIt(content: StringMap): Either[IOError, String] = UpickleUtils.writeE(content)

  override def deserialize(ser: String): Either[IOError, AnyRef] = UpickleUtils.readE[StringMap](ser)

}