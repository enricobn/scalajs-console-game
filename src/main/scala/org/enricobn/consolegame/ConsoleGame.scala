package org.enricobn.consolegame

import java.util.UUID

import org.enricobn.consolegame.commands.MessagesCommand
import org.enricobn.consolegame.content.{Messages, MessagesSerializer}
import org.enricobn.shell.VirtualShellContext
import org.enricobn.shell.impl._
import org.enricobn.terminal.{CanvasInputHandler, CanvasTextScreen, JSLogger, TerminalImpl}
import org.enricobn.vfs.impl.VirtualUsersManagerImpl
import org.enricobn.vfs.inmemory.InMemoryFS
import org.enricobn.vfs.utils.Utils
import org.enricobn.vfs._
import org.scalajs.dom
import org.scalajs.dom.FileReader
import org.scalajs.dom.html.{Anchor, Canvas, Input}
import org.scalajs.dom.raw._

import scala.collection.GenTraversableOnce
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportAll

// to access members of structural types (new {}) without warnings
import scala.language.reflectiveCalls

/**
  * Created by enrico on 12/8/16.
  */
@JSExportAll
abstract class ConsoleGame[GS <: GameState[GSS], GSS <: AnyRef]
    (mainCanvasID: String, messagesCanvasID: String, loadGameID: String, saveGameID: String, gameStateFactory: GameStateFactory[GS, GSS]) {
  private var globalGameState = GlobalGameStateFactory.create()
  private var userGameState = gameStateFactory.create()
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
  private var fs = new InMemoryFS(vum)
  protected val context: VirtualShellContext = new VirtualShellContextImpl()
  protected def root: VirtualFolder = fs.root
  private var shell = new VirtualShell(mainTerminal, vum, context, root)
  private val messagesShell = new VirtualShell(messagesTerminal, vum, context, root)

  private var userCommands : Seq[VirtualFile] = _

  def start() {
    mainCanvas.contentEditable = "true"
    mainCanvas.focus()

    val init = for {
      _ <- vum.addUser("guest", guestPassword).toLeft(()).right
      _ <- initFS().right
      _ <- initUserGameState(userGameState).toLeft(()).right
      _ <- initGlobalGameState().toLeft(()).right
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
//    val globalSer = globalGameState.toSerializable
//    val userSer = userGameState.toSerializable
    val serializers: Map[Class[_], Serializer] = (getSerializers() ++ geyGlobalSerializers).map(serializer =>
      (serializer.clazz, serializer)
    ).toMap

    val job =
      for {
        fileContents <- lift(
          allFiles.map(file => (file, file.content))
        ).right
        fileContentSerializers <- Right(allSome(fileContents.map {case (file, content) => ((file,content), serializers.get(content.getClass))})).right
        serializedContents <- lift(
          fileContentSerializers.map { case ((file, content), serializer) => ((file, serializer), serializer.serialize(content))
        }).right
        files <- Right(serializedContents.map { case ((file, serializer), ser) =>
          SerializedFile(file.path, file.owner, file.permissions.octal, serializer.name, ser)
        }).right
        folders <- Right(getAllFolders(root).map(folder => SerializedFolder(folder.path, folder.owner, folder.permissions.octal))).right
        ser <- GameState.writeE(SerializedFS(folders, files)).right
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


//    val ser = GlobalAndUserSerializableGameState(globalSer, userSer)
//
//    GameState.writeE(ser) match {
//      case Left(error) => dom.window.alert(s"Error saving game: ${error.message}.")
//      case Right(s) =>
//        val file = new Blob(js.Array(s), BlobPropertyBag("text/plain"))
//        anchor.href = URL.createObjectURL(file)
//        anchor.pathname = "consolegame.json"
//        messagesTerminal.add(s"Game saved.\n")
//        messagesTerminal.flush()
//    }
  }

  private def geyGlobalSerializers = {
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

    val serializers: Map[String, Serializer] = (getSerializers() ++ geyGlobalSerializers).map(serializer =>
      (serializer.name, serializer)
    ).toMap

    fs = new InMemoryFS(vum)

    val deserialize = for {
      serializedFS <- GameState.readE[SerializedFS](resultContent).right
      // I sort them so I crete them in order
      _ <- Utils.lift(serializedFS.folders.sortBy(_.path).map(serializedFolder => {
        println(serializedFolder.path)
        for {
          // TODO the path is absolute, I must make all intermediate folders
          folder <- mkdir(serializedFolder.path).right
          _ <- folder.chown(serializedFolder.owner).toLeft(()).right
          result <- folder.chmod(serializedFolder.permissions).toLeft(()).right
        } yield {
          result
        }
      })).right
      serializedAndSerializers <- lift(serializedFS.files.map(serializedFile => {
        val serializerE = serializers.get(serializedFile.serializerName)
          .toRight(IOError(s"Cannot find serializer with name=${serializedFile.serializerName}"))
        (serializedFile, serializerE)
      })).right
      serializedAndSerializerAndContent <- lift(serializedAndSerializers.map {case (serializedFile, serializer) =>
        // TODO errors
        val path = VirtualPath(serializedFile.path)
        val file = path.parentFragments.get.toFolder(root).right.get.touch(path.name).right.get
        println(file)
        file.chown(serializedFile.owner)
        file.chmod(serializedFile.permissions)
        println(serializedFile.ser)
        ((serializedFile, serializer), serializer.deserialize(serializedFile.ser))
      }).right
      contentFiles <- lift(serializedAndSerializerAndContent.map {case ((serializedFile, serializer), content) =>
        (content, getFile(serializedFile.path))
      }).right
      result <- Utils.mapFirstSome[(AnyRef, VirtualFile), IOError](contentFiles,
        { case (content, file) => file.content = content }
      ).toLeft(()).right
    } yield {
      result
    }

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
        case Right(showPrompt) => {
          // TODO even the fs must be set here, but some methods takes the current fs and root
          shell.stop()
          shell = new VirtualShell(mainTerminal, vum, context, root)
          // TODO error
          shell.currentFolder = root.resolveFolderOrError("/home/guest", "Cannot find /home/guest").right.get
          mainTerminal.add("\u001b[2J\u001b[1;1H") // clear screen an reset cursor to 1, 1
          shell.start()
          showPrompt
        }
      }

    })

//      messagesShell.stopInteractiveCommands({ () =>
//
//        val run = for {
//          _ <- userGameState.delete().toLeft(()).right
//          _ <- globalGameState.delete().toLeft(()).right
//          _ <- deleteUserCommands().toLeft(()).right
//          globalAndUser <- GameState.readE[GlobalAndUserSerializableGameState](content).right
//          userGameState <- gameStateFactory.deserialize(globalAndUser.user, fs).right
//          globalGameState <- GlobalGameStateFactory.deserialize(globalAndUser.global, fs).right
//          userCommands <- createUserCommands().right
//          showPrompt <- messagesShell.run(MessagesCommand.NAME).right
//          _ <- vum.logUser("guest", guestPassword).toLeft(()).right
//        } yield {
//          this.userGameState = userGameState
//          this.globalGameState = globalGameState
//          this.userCommands = userCommands
//
//          messagesTerminal.add(s"Game loaded from ${f.name}\n")
//          messagesTerminal.flush()
//
//          showPrompt
//        }
//
//        run match {
//          case Left(error) =>
//            dom.window.alert(s"Error loading game: ${error.message}.")
//            false
//          case Right(showPrompt) => showPrompt
//        }
//      })
    // TODO Error
    vum.logUser("guest", guestPassword)
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

  def initUserGameState(gameState: GS): Option[IOError]

  def createUserCommands(): Either[IOError,Seq[VirtualFile]]

  def getSerializers() : Seq[Serializer]

  private def deleteUserCommands(): Option[IOError] = Utils.mapFirstSome(userCommands, GameState.delete)

  private def initGlobalGameState(): Option[IOError] = {

    val messages = new Messages()

    val job = for {
      log <- root.resolveFolderOrError("/var/log", "Cannot find folder /var/log.").right
      messagesFile <- log.touch("messages.log").right
      _ <- (messagesFile.content = messages).toLeft(None).right
    } yield {
      globalGameState.setMessages(messagesFile, messages)
    }

    job.left.toOption

  }

  private case class GlobalAndUserSerializableGameState(global: GlobalSerializableGameState, user: GSS)

  // TODO error
  private def allFiles: Set[VirtualFile] = {
    vum.logRoot(rootPassword)
    val files = getAllFiles(root)
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


  // TODO move to library ?
  private def lift[T,TL,TR](xs: GenTraversableOnce[(T, Either[TL,TR])]) : Either[TL,List[(T,TR)]] =
    xs.foldRight(Right(List.empty[(T,TR)]) : Either[TL,List[(T,TR)]]) { (value, result) => {
      result match {
        case Left(_) => result
        case Right(r) =>
          value match {
            case (t,Left(l)) => Left(l)
            case (t,Right(r1)) => Right( (t,r1) :: r)
          }
      }
    }}

  // TODO move to library ?
  private def lift[T,T1](xs: GenTraversableOnce[(T, Option[T1])]) : Option[List[(T,T1)]] =
    xs.foldRight(Some(List.empty) : Option[List[(T,T1)]])((value, result) => {
      result match {
        case Some(l) =>
          value match {
            case (t, Some(v)) => Some((t,v) :: l)
            case _ => None
          }
        case _ => None
      }
    })

  // TODO move to library ?
  private def allSome[T,T1](xs: GenTraversableOnce[(T, Option[T1])]) : List[(T,T1)] =
    xs.foldRight(List.empty : List[(T,T1)])((value, result) => {
        value match {
          case (t, Some(v)) => (t,v) :: result
          case _ => result
        }
    })

  private def getFile(path: String): Either[IOError, VirtualFile] = {
    VirtualPath(path).toFile(root)
  }

  private def mkdir(path: String) : Either[IOError, VirtualFolder] = {
    val virtualPath = VirtualPath(path)

    virtualPath.parentFragments.get.toFolder(fs.root) match {
      case error@Left(_) => error
      case Right(parent) => parent.mkdir(virtualPath.name)
    }
  }
}

private case class SerializedFile(path: String, owner: String, permissions: Int, serializerName: String, ser: String)
private case class SerializedFolder(path: String, owner: String, permissions: Int)
private case class SerializedFS(folders: List[SerializedFolder], files: List[SerializedFile])