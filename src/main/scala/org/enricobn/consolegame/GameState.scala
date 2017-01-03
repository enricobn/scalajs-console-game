package org.enricobn.consolegame

import org.enricobn.vfs.impl.VirtualAbsolutePath
import org.enricobn.vfs.utils.Lift
import org.enricobn.vfs.{IOError, VirtualFS, VirtualFile}

import scala.collection.mutable
import upickle.default._
import Lift._

import scala.collection.mutable.ArrayBuffer

/**
  * Created by enrico on 12/30/16.
  */

case class SerializedContent(path: String, owner: String, permissions: Int, className: String, content: String)

class GameStateFactory {
  private val classRegistry = new mutable.HashMap[String, ContentSerializer[AnyRef]]()

  def register[T <: AnyRef](contentSerializer: ContentSerializer[T]): Unit = {
    classRegistry.put(contentSerializer.clazz.getName, contentSerializer.asInstanceOf[ContentSerializer[AnyRef]])
  }

  def load(s: String, fs: VirtualFS) : Either[IOError, GameState] = {
    val ser = read[Iterable[SerializedContent]](s)
    val gameState = new GameState()
    val filesE = ser.map(c => {
      val serializer = classRegistry(c.className)
      val deserialized = deserialize(c.content, serializer)
      val path = VirtualAbsolutePath(c.path)
      for {
        parent <- path.parent.toRight(IOError(s"No parent found for $path")).right
        folderO <- fs.root.resolveFolder(parent.path).right
        folder <- folderO.toRight(IOError(s"Cannot find folder $parent")).right
        file <- folder.touch(path.name).right
        _ <- (file.content = deserialized).toLeft(None).right
        _ <- file.chmod(c.permissions).toLeft(None).right
        _ <- file.chown(c.owner).toLeft(None).right
      } yield file
    })

    lift(filesE) match {
      case Left(error) => Left(error)
      case Right(files) =>
        files.foreach(gameState.add)
        Right(gameState)
    }
  }

  def save(gameState: GameState) : Either[IOError, String] = {
    val serE = gameState.contents.map {file =>
      for {
        content <- file.content.right
      } yield {
        val className: String = content.getClass.getName
        val serializer = classRegistry(className)

        SerializedContent(file.path, file.owner, file.permissions.octal, className, serialize(content, serializer))
      }
    }

    lift(serE) match {
      case Left(error) => Left(error)
      case Right(ser) => Right(write(ser))
    }
  }

  private def serialize[T <: AnyRef](value: T, contentSerializer: ContentSerializer[T]) : String =
    contentSerializer.toString(value)

  private def deserialize[T <: AnyRef](s: String, contentSerializer: ContentSerializer[T]) : T =
    contentSerializer.fromString(s)

}

case class SerializableGameState(contents: Map[String, AnyRef])

class GameState() {
  private val _contents = new ArrayBuffer[VirtualFile]()

  def add(file: VirtualFile): Unit = {
    _contents += file
  }

  def contents = _contents.toList

}
