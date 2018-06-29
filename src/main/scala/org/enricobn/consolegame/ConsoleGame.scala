package org.enricobn.consolegame

import java.util.UUID

import org.enricobn.consolegame.commands.MessagesCommand
import org.enricobn.consolegame.content.{Messages, MessagesSerializer, SimpleSerializer}
import org.enricobn.shell.impl._
import org.enricobn.shell.{VirtualCommand, VirtualCommandOperations}
import org.enricobn.terminal._
import org.enricobn.vfs._
import org.scalajs.dom
import org.scalajs.dom.FileReader
import org.scalajs.dom.html.{Anchor, Button, Canvas, Input}
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

  def initFS(fs: UnixLikeInMemoryFS, userName: String, allCommands : Either[IOError, Seq[VirtualCommand]])
            (implicit authentication: Authentication) : Either[IOError,Unit] =
    for {
      _ <- VirtualCommandOperations.createCommandFiles(fs.bin, new LsCommand, new CdCommand, new CatCommand).right
      userCommands <- allCommands.right
      _ <- VirtualCommandOperations.createCommandFiles(fs.usrBin, userCommands :_*).right
      messagesLog <- fs.varLog.findFile("messages.log").right
      messagesFile <- (if (messagesLog.isDefined) { Right(messagesLog.get) } else { fs.varLog.createFile("messages.log", Messages(Seq.empty)) }).right
      // TODO I don't like that user has the ability to delete the file, remove logs etc...
      // But it's not easy to avoid that and grant access to add messages, perhaps I need some
      // system call handling, but I think it's too complicated for this project ...
      _ <- messagesFile.chown(userName).toLeft(()).right
    } yield ()

  val globalSerializers : Seq[Serializer] = List(MessagesSerializer, StringListSerializer, StringMapSerializer)

  def newMainShell(rootPassword: String, mainTerminal: Terminal) : Either[IOError, VirtualShell] =
    for {
      fs <- UnixLikeInMemoryFS(rootPassword).right
      rootAuthentication <- fs.vum.logRoot(rootPassword).right
    } yield UnixLikeVirtualShell(fs, mainTerminal, fs.root, rootAuthentication)

}

/**
  * Created by enrico on 12/8/16.
  */
@JSExportAll
abstract class ConsoleGame(mainCanvasID: String, messagesCanvasID: String, newGameID: String, loadGameID: String, saveGameID: String) {

  import org.enricobn.consolegame.ConsoleGame._

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
  private var rootAuthentication: Authentication = _
  private var shell: VirtualShell = _
  private var messagesShell: VirtualShell = _
  private var userName : String = _

  private val newGameButton = dom.document.getElementById(newGameID).asInstanceOf[Button]
  private val loadGame = dom.document.getElementById(loadGameID).asInstanceOf[Input]
  private val saveGameAnchor = dom.document.getElementById(saveGameID).asInstanceOf[Anchor]

  newGameButton.onclick = onNewGame _
  loadGame.addEventListener("change", readGame(loadGame) _, false)
  saveGameAnchor.onclick = onSaveGame(saveGameAnchor) _

  def onNewGame(shell: VirtualShell): Option[IOError]

  def commands : Either[IOError, Seq[VirtualCommand]]

  def getSerializers: Seq[Serializer]

  private def onNewGame(event: MouseEvent) {
    mainCanvas.contentEditable = "true"
    mainCanvas.focus()

    mainTerminal.screen.clear(true)

    mainTerminal.add("Your name: ")
    mainTerminal.flush()

    newMainShell(rootPassword, mainTerminal) match {
      case Right(newShell) => newShell.readLine({ s => newGame(s, newShell) })
      case Left(error) => dom.window.alert(s"Error initializing: ${error.message}")
    }

  }

  private def newGame(newUserName: String, newShell: VirtualShell) {
    implicit val authentication: Authentication = newShell.authentication

    import org.enricobn.vfs.utils.Utils.RightBiasedEither

    // TODO error
    val newFs = newShell.fs.asInstanceOf[UnixLikeInMemoryFS]

    val runInit = for {
      _ <- newFs.vum.addUser(newUserName, userPassword).toLeft(())
      _ <- ConsoleGame.initFS(newFs, newUserName, allCommands)
      userHome <- newShell.toFolder("/home/" + newUserName)
      newMessagesShell = UnixLikeVirtualShell(newFs, messagesTerminal, newFs.root, rootAuthentication)
      _ <- newMessagesShell.login(newUserName, userPassword)
      _ <- newShell.login(newUserName, userPassword)
      _ <- onNewGame(newShell).toLeft(())
    } yield {
      (userHome, newMessagesShell)
    }

    runInit match {
      case Left(error) => dom.window.alert(s"Error initializing: ${error.message}")
      case Right((userHome, newMessageShell)) =>
        if (shell != null) {
          shell.stop()
        }

        if (messagesShell != null) {
          messagesShell.stopInteractiveCommands(() => false)
          messagesShell.stop()
        }

        userName = newUserName
        if (fs != null) {
          fs.notifier.shutdown()
        }
        fs = newFs
        vum = fs.vum
        shell = newShell
        shell.currentFolder = userHome
        shell.start()

        messagesShell = newMessageShell
        messagesShell.startWithCommand(MessagesCommand.NAME)
    }
  }

  private def onSaveGame(anchor: Anchor)(evt: MouseEvent): Unit = {
    import org.enricobn.vfs.utils.Utils.RightBiasedEither
    val job =
      for {
        serializedFS <- SerializedFSOperations.build(allFiles, getAllFolders(fs.root), getSerializersMap)(rootAuthentication)
        game = SerializedGame(userName, serializedFS)
        ser <- UpickleUtils.writeE(game)
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

  private def getGlobalSerializers = ConsoleGame.globalSerializers

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

    if (messagesShell != null) {

      messagesShell.stopInteractiveCommands({ () =>
        loadGame(f, r.result.toString)
      })
    } else {
      loadGame(f, r.result.toString)
    }

  }

  private def loadGame(f: File, resultContent: String): Boolean = {
    // TODO error handling
    val newFs = UnixLikeInMemoryFS(rootPassword).right.get

    val newRootAuthentication = newFs.vum.logRoot(rootPassword).right.get

    val newShell = UnixLikeVirtualShell(newFs, mainTerminal, newFs.root, newRootAuthentication)

    implicit val authentication : Authentication = newRootAuthentication

    println("stopInteractiveCommands")

    val run = for {
      serializedGame <- UpickleUtils.readE[SerializedGame](resultContent).right
      _ <- newFs.vum.addUser(serializedGame.userName, userPassword).toLeft(()).right
      _ <- SerializedFSOperations.load(newShell, getSerializersMap, serializedGame.fs).right
      //showPrompt <- messagesShell.run(MessagesCommand.NAME).right
      _ <- ConsoleGame.initFS(newFs, serializedGame.userName, allCommands).right
      _ <- newShell.login(serializedGame.userName, userPassword).right
    } yield {
      (false, serializedGame.userName)
    }

    run match {
      case Left(error) =>
        println(error.message)
        dom.window.alert("Error loading game. See javascript console for details.")
        false
      case Right((showPrompt, newUserName)) =>
        try {
          messagesTerminal.add(s"Game loaded from ${f.name}\n")
          messagesTerminal.flush()

          if (fs != null) {
            fs.notifier.shutdown()
          }

          fs = newFs
          vum = newFs.vum
          rootAuthentication = newRootAuthentication
          userName = newUserName

          // TODO error
          val homeFolder = newShell.toFolder("/home/" + userName).right.get

          if (messagesShell != null) {
            messagesShell.stop()
          }
          messagesShell = UnixLikeVirtualShell(newFs, messagesTerminal, homeFolder, newRootAuthentication)

          // TODO Error
          messagesShell.login(userName, userPassword)

          messagesShell.startWithCommand(MessagesCommand.NAME)

          if (shell != null) {
            shell.stop()
          }
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
  }

  private def allCommands : Either[IOError, Seq[VirtualCommand]] = {
    for {
      defCommands <- Right(defaultCommands).right
      commands <- commands.right
    } yield defCommands ++ commands
  }

  private def defaultCommands : Seq[VirtualCommand] = Seq(new MessagesCommand())

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

case class SerializedGame(userName: String, fs: SerializedFS)