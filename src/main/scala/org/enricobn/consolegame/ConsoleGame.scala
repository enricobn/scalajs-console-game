package org.enricobn.consolegame

import org.enricobn.buyandsell.content.{GameInfo, GameInfoSerializer}
import org.enricobn.consolegame.commands.MessagesCommand
import org.enricobn.consolegame.content.{Messages, MessagesSerializer, PasswdSerializer, SimpleSerializer}
import org.enricobn.shell.VirtualCommandOperations
import org.enricobn.shell.impl.*
import org.enricobn.terminal.*
import org.enricobn.vfs.*
import org.enricobn.vfs.impl.{VirtualSecurityManagerImpl, VirtualUsersManagerFileImpl}
import org.enricobn.vfs.inmemory.InMemoryFS

import scala.compiletime.uninitialized
import scala.scalajs.js.annotation.JSExportAll
import scala.util.Random.nextInt

// to access members of structural types (new {}) without warnings
import org.enricobn.vfs.IOError.*
import upickle.default.{macroRW, ReadWriter as RW}

import scala.language.reflectiveCalls

object ConsoleGame {
  private val group = "game"

  // TODO move in VirtualFile?
  private def delete(file: VirtualFile)(implicit authentication: Authentication): Either[IOError, Unit] =
    file.parent match {
      case Some(folder) => folder.deleteFile(file.name)
      case _ => "No parent".ioErrorE
    }

  private def initFS(fs: UnixLikeInMemoryFS, userName: String, allCommands: Either[IOError, Seq[GameCommand]])
                    (implicit authentication: Authentication): Either[IOError, Unit] =
    for {
      _ <- VirtualCommandOperations.createCommandFiles(fs.bin, LsCommand, CdCommand, CatCommand)
      userCommands <- allCommands
      _ <- VirtualCommandOperations.createCommandFiles(fs.usrBin, userCommands.map(_.virtualCommand) *)
      messagesLog <- fs.varLog.findFile("messages.log")
      messagesFile <-
        if (messagesLog.isDefined) {
          Right(messagesLog.get)
        } else {
          fs.varLog.createFile("messages.log", Messages(Seq.empty))
        }
      _ <- messagesFile.chown(userName)
      gameInfo <- GameInfo.get(fs)
      _ <- gameInfo.setContent(GameInfo(userName))
    } yield ()

  val globalSerializers: Seq[Serializer] = List(MessagesSerializer, StringListSerializer, StringMapSerializer,
    GameInfoSerializer, PasswdSerializer)

  private[consolegame] def createMainShell(rootPassword: String, mainTerminal: Terminal, scheduler: Scheduler): Either[IOError, VirtualShell] = {
    val _fs = InMemoryFS(
      {
        VirtualUsersManagerFileImpl(_, rootPassword).toOption.get
      },
      { (_, vum) => new VirtualSecurityManagerImpl(vum) })
    for {
      fs <- UnixLikeInMemoryFS(_fs, rootPassword)
      rootAuthentication <- fs.vum.logRoot(rootPassword)
    } yield UnixLikeVirtualShell(fs, mainTerminal, fs.root, rootAuthentication, scheduler)
  }

  private def clearScreen(terminal: Terminal): Unit = {
    terminal.add("\u001b[2J\u001b[1;1H") // clear screen an reset cursor to 1, 1
    terminal.flush()
  }

}

/**
  * Created by enrico on 12/8/16.
  */
@JSExportAll
abstract class ConsoleGame(mainTerminal: Terminal, messagesTerminal: Terminal, logger: JSLogger, scheduler: Scheduler) {

  import org.enricobn.consolegame.ConsoleGame.*

  private var rootPassword = nextInt().toString
  private var userPassword = nextInt().toString
  private var fs: UnixLikeInMemoryFS = uninitialized
  private var vum: VirtualUsersManager = uninitialized
  private var rootAuthentication: Authentication = uninitialized
  private[consolegame] var shell: VirtualShell = uninitialized
  private var messagesShell: VirtualShell = uninitialized
  private var userName: String = uninitialized

  protected def onNewGame(shell: VirtualShell): Either[IOError, Unit]

  /**
    *
    * @return Either an error or a sequence of pairs. The boolean indicates if the command should be run by the user
    */
  protected def commands: Either[IOError, Seq[GameCommand]]

  protected def getSerializers: Seq[Serializer]

  /**
    * an optional command to run in background when starting the shell
    *
    * @return an option pair of command name and arguments
    */
  protected def getBackgroundCommand: Option[(String, List[String])]

  def executeLater(runnable: () => Unit): Unit

  def showError(message: String): Unit

  def saveToFile(content: String, fileName: String): Unit

  protected def createUserWithRandomPassword(user: String): Either[IOError, VirtualShell] = {
    val userTerminal = new FakeTerminal
    val userShell = UnixLikeVirtualShell(shell.fs.asInstanceOf[UnixLikeInMemoryFS], userTerminal, shell.fs.root, rootAuthentication,
      scheduler)
    val password = nextInt().toString

    userTerminal.setShell(userShell)

    for {
      _ <- shell.vum.addUser(user, password, group)(rootAuthentication)
      _ <- userShell.login(user, password)
    } yield userShell
  }

  private[consolegame] def onNewGame(): Unit = {
    clearScreen(mainTerminal)

    mainTerminal.add("User name: ")
    mainTerminal.flush()

    createMainShell(rootPassword, mainTerminal, scheduler) match {
      case Right(newMainShell) =>
        if (shell != null) {
          shell.stop(rootAuthentication)
        }

        // executeLater since newGame adds an input handler to the terminal, but since it runs inside another input handler
        // (created by the readLine) the one added goes after that and the enter key is processed!
        newMainShell.readLine { s => executeLater { () => newGame(s, newMainShell) } }
      case Left(error) => showError(s"Error initializing: ${error.message}")
    }

  }

  private def newGame(newUserName: String, newMainShell: VirtualShell): Unit = {

    implicit val authentication: Authentication = newMainShell.authentication
    rootAuthentication = authentication

    // TODO error
    val newFs = newMainShell.fs.asInstanceOf[UnixLikeInMemoryFS]

    val runInit = for {
      _ <- newFs.vum.addUser(newUserName, userPassword, group)
      _ <- ConsoleGame.initFS(newFs, newUserName, allCommands)
      userHome <- newMainShell.toFolder("/home/" + newUserName)
      newMessagesShell = UnixLikeVirtualShell(newFs, messagesTerminal, newFs.root, rootAuthentication, scheduler)
      _ <- newMessagesShell.login(newUserName, userPassword)
      _ <- newMainShell.login(newUserName, userPassword)
    } yield {
      (userHome, newMessagesShell)
    }

    runInit match {
      case Left(error) => showError(s"Error initializing: ${error.message}")
      case Right((userHome, newMessageShell)) =>
        if (messagesShell != null) {
          messagesShell.stop(rootAuthentication)
        }

        userName = newUserName

        if (fs != null) {
          fs.notifier.shutdown()
        }

        fs = newFs
        vum = fs.vum
        shell = newMainShell
        shell.currentFolder = userHome

        messagesShell = newMessageShell
        messagesShell.startWithCommand(background = false, MessagesCommand.NAME)

        executeLater(() => {
          onNewGame(shell).fold({ e =>
            showError(s"Error initializing game $e")
          }, { _ =>
            getBackgroundCommand match {
              case Some(x) => shell.startWithCommand(true, x._1, x._2 *)
              case None => shell.start()
            }
            changePermissionOfPrivateCommands.left.foreach(error => showError(s"Error changing permissions of start commands. $error"))
          })
        })
    }
  }

  private def changePermissionOfPrivateCommands(implicit authentication: Authentication) = {
    val privatePermission = VirtualPermissionsImpl(VirtualPermission.NONE, VirtualPermission.NONE, VirtualPermission.NONE)

    for {
      allCommands <- allCommands
      privateCommands = allCommands.filter(!_.visible).map(_.virtualCommand)
      _ <- Right(for (command <- privateCommands) yield {
        for {
          file <- fs.usrBin.findFileOrError(command.name)
          _ <- file.setPermissions(privatePermission)
        } yield ()
      })
    } yield ()
  }

  private[consolegame] def onSaveGame(): Unit = {
    val job =
      for {
        serializedFS <- SerializedFSOperations.build(allFiles, getAllFolders(fs.root), getSerializersMap)(rootAuthentication)
        game = SerializedGame(userName, serializedFS)
        ser <- UpickleUtils.writeE(game, prettyFormat = true)
      } yield {
        val fileName = "consolegame.json"
        saveToFile(ser, fileName)
        messagesTerminal.add(s"Game saved.\n")
        messagesTerminal.flush()
      }

    job match {
      case Left(error) => showError(s"Error saving game: ${error.message}.")
      case _ =>
    }

  }

  private def getSerializersMap: Map[String, Serializer] = {
    val serializers: Map[String, Serializer] = (getSerializers ++ getGlobalSerializers).map(serializer =>
      (serializer.clazz.getName, serializer)
    ).toMap
    serializers
  }

  private def getGlobalSerializers = ConsoleGame.globalSerializers

  private[consolegame] def loadGame(fileName: String, resultContent: String): Boolean = {
    if (messagesShell != null) {
      messagesShell.stop(rootAuthentication)
    }

    val run = for {
      serializedGame <- UpickleUtils.readE[SerializedGame](resultContent)
      passwd <- serializedGame.fs.files.find(_.path == "/etc/passwd").toRight(IOError("cannot find passwd file"))
        .flatMap(content => PasswdSerializer.deserialize(content.ser))
      inMemoryFs = InMemoryFS(fs => VirtualUsersManagerFileImpl(fs, passwd).toOption.get,
        (_, vum) => new VirtualSecurityManagerImpl(vum))
      newRootAuthentication = passwd.users.find(_.user == VirtualUsersManager.ROOT).get.auth
      newRootPassword = passwd.users.find(_.user == VirtualUsersManager.ROOT).get.password
      newUserPassword = passwd.users.find(_.user == serializedGame.userName).get.password
      newFs <- UnixLikeInMemoryFS(inMemoryFs, newRootPassword)
      newShell = UnixLikeVirtualShell(newFs, mainTerminal, newFs.root, newRootAuthentication, scheduler)
      _ <- SerializedFSOperations.load(newShell, getSerializersMap, serializedGame.fs)(newRootAuthentication)
      _ <- ConsoleGame.initFS(newFs, serializedGame.userName, allCommands)(newRootAuthentication)
      _ <- newShell.login(serializedGame.userName, newUserPassword)
    } yield {
      (false, serializedGame.userName, newFs, newShell, newRootAuthentication, newRootPassword, newUserPassword)
    }

    run match {
      case Left(error) =>
        showError(s"Error loading game $error")
        false
      case Right((showPrompt, newUserName, newFs, newShell, newRootAuthentication, newRootPassword, newUserPassword)) =>
        try {
          clearScreen(messagesTerminal)

          messagesTerminal.add(s"Game loaded from $fileName\n")
          messagesTerminal.flush()

          if (fs != null) {
            fs.notifier.shutdown()
          }

          // TODO error
          val homeFolder = newShell.toFolder("/home/" + newUserName).toOption.get

          if (messagesShell != null) {
            messagesShell.stop(rootAuthentication)
          }
          messagesShell = UnixLikeVirtualShell(newFs, messagesTerminal, homeFolder, newRootAuthentication, scheduler)

          // TODO Error
          messagesShell.login(newUserName, userPassword)

          messagesShell.startWithCommand(background = false, MessagesCommand.NAME)

          if (shell != null) {
            shell.stop(rootAuthentication)
          }

          fs = newFs
          vum = newFs.vum
          rootAuthentication = newRootAuthentication
          rootPassword = newRootPassword
          userName = newUserName
          userPassword = newUserPassword

          shell = newShell
          shell.currentFolder = homeFolder

          clearScreen(mainTerminal)

          getBackgroundCommand match {
            case Some(x) => shell.startWithCommand(true, x._1, x._2 *)
            case None => shell.start()
          }
        } catch {
          case e: Exception =>
            e.printStackTrace()

            showError("Error loading game. See javascript console for details.")

            throw e
        }

        showPrompt
    }
  }

  private def allCommands: Either[IOError, Seq[GameCommand]] = {
    for {
      defCommands <- Right(defaultCommands)
      commands <- commands
    } yield defCommands ++ commands
  }

  private def defaultCommands: Seq[GameCommand] = Seq(GameCommand(new MessagesCommand(), visible = false))

  // TODO error for logging (even for getAllFiles?)
  private def allFiles: Seq[VirtualFile] = {
    implicit val authentication: Authentication = rootAuthentication

    vum.logRoot(rootPassword)
    val files = getAllFiles(fs.root)
    vum.logUser(userName, userPassword)
    files
  }

  // TODO create scalajs-vfs VirtualFolder.getAllFiles
  // TODO error?
  private def getAllFiles(folder: VirtualFolder): Seq[VirtualFile] = {
    implicit val authentication: Authentication = rootAuthentication

    (for {
      files <- folder.files
      folders <- folder.folders
    } yield {
      files ++ folders.flatMap(getAllFiles)
    }) match {
      case Left(_) => Seq()
      case Right(files) => files
    }
  }

  // TODO create scalajs-vfs VirtualFolder.getAllFolders
  // TODO error?
  private def getAllFolders(folder: VirtualFolder): List[VirtualFolder] = {
    implicit val authentication: Authentication = rootAuthentication

    (for {
      folders <- folder.folders
    } yield {
      folders.toList ++ folders.flatMap(getAllFolders)
    }) match {
      case Left(_) => List()
      case Right(folders) => folders
    }
  }

}

case class SerializedFile(path: String, owner: String, group: String, permissions: Int, serializerName: String, ser: String)

case class SerializedFolder(path: String, owner: String, group: String, permissions: Int)

case class SerializedFS(folders: List[SerializedFolder], files: List[SerializedFile])

object StringListSerializer extends SimpleSerializer(classOf[StringList]) {
  implicit val rw: RW[StringList] = macroRW

  override def serializeIt(content: StringList): Either[IOError, String] = UpickleUtils.writeE(content)

  override def deserialize(ser: String): Either[IOError, AnyRef] = UpickleUtils.readE[StringList](ser)

}

object StringMapSerializer extends SimpleSerializer(classOf[StringMap]) {
  implicit val rw: RW[StringMap] = macroRW

  override def serializeIt(content: StringMap): Either[IOError, String] = UpickleUtils.writeE(content)

  override def deserialize(ser: String): Either[IOError, AnyRef] = UpickleUtils.readE[StringMap](ser)

}

object SerializedGame {
  implicit val rw: RW[SerializedGame] = macroRW
  implicit val rwfs: RW[SerializedFS] = macroRW
  implicit val rwfolder: RW[SerializedFolder] = macroRW
  implicit val rwfile: RW[SerializedFile] = macroRW
}

case class SerializedGame(userName: String, fs: SerializedFS)