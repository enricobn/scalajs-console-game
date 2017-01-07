package org.enricobn.consolegame

import org.enricobn.vfs.impl.VirtualAbsolutePath
import org.enricobn.vfs.utils.Lift
import org.enricobn.vfs.{IOError, VirtualFS, VirtualFile}

import scala.collection.mutable
import upickle.default._
import Lift._
import org.enricobn.consolegame.content.Messages

import scala.collection.mutable.ArrayBuffer

import IOError._

import Messages._

/**
  * Created by enrico on 12/30/16.
  */

case class SerializedContent[T <: AnyRef](path: String, owner: String, permissions: Int, content: T) {
  def deserialize(fs: VirtualFS) = {
    val vPath = VirtualAbsolutePath(path)
    for {
      parent <- vPath.parent.toRight(IOError(s"No parent found for $path")).right
      folderO <- fs.root.resolveFolder(parent.path).right
      folder <- folderO.toRight(IOError(s"Cannot find folder $parent")).right
      file <- folder.touch(vPath.name).right
      _ <- (file.content = content).toLeft(None).right
      _ <- file.chmod(permissions).toLeft(None).right
      _ <- file.chown(owner).toLeft(None).right
    } yield FileContent(file, content)
  }
}

case class FileContent[T <: AnyRef](file: VirtualFile, content: T) {
  def serialize() : SerializedContent[T] = SerializedContent(file.path, file.owner, file.permissions.octal, content)
}

object GameStateFactory {
  def writeE[T1: Writer](value: T1) : Either[IOError, String] = {
    try {
      Right(write(value))
    } catch {
      case e: Throwable => e.getMessage.ioErrorE
    }
  }

  def readE[T1: Reader](s: String) : Either[IOError, T1] = {
    try {
      Right(read[T1](s))
    } catch {
      case e: Throwable => e.getMessage.ioErrorE
    }

  }
}

class GameStateFactory {
  private val classRegistry = new mutable.HashMap[String, ContentSerializer[AnyRef]]()

  def register[T <: AnyRef](contentSerializer: ContentSerializer[T]): Unit = {
    classRegistry.put(contentSerializer.clazz.getName, contentSerializer.asInstanceOf[ContentSerializer[AnyRef]])
  }

  def load(s: String, fs: VirtualFS) : Either[IOError, GameState] = {
    for {
      ser <- GameStateFactory.readE[SerializableGameState](s).right
      messages <- ser.messages.deserialize(fs).right
    } yield {
      val result = new GameState()
      result.setMessages(messages.file, messages.content)
      result
    }
  }

  def save(gameState: GameState) : Either[IOError, String] = {
//    Right(write(gameState.serialize()))
    GameStateFactory.writeE[SerializableGameState](gameState.serialize())
  }

  private def serialize[T <: AnyRef](value: T, contentSerializer: ContentSerializer[T]) : Either[IOError, String] =
    contentSerializer.toString(value)

  private def deserialize[T <: AnyRef](s: String, contentSerializer: ContentSerializer[T]) : Either[IOError, T] =
    contentSerializer.fromString(s)

}

case class SerializableGameState(messages: SerializedContent[Messages])

class GameState() {
  private var messages: FileContent[Messages] = null

  def setMessages(file: VirtualFile, messages: Messages): Unit = {
    this.messages = FileContent(file, messages)
  }

  def serialize() = SerializableGameState(messages.serialize())

  def create() = {

  }

  def delete() = {
    messages.file.parent.deleteFile(messages.file.name)
  }

}
