package org.enricobn.consolegame

import org.enricobn.consolegame.content.{Messages, Warehouse}
import org.enricobn.vfs.IOError._
import org.enricobn.vfs.impl.VirtualAbsolutePath
import org.enricobn.vfs.{IOError, VirtualFS, VirtualFile}
import upickle.default._
import Messages._
import Warehouse._
import org.enricobn.vfs.utils.Utils

import scala.collection.mutable.ArrayBuffer

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

object GameState {

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

  def load(s: String, fs: VirtualFS) : Either[IOError, GameState] = {
    for {
      ser <- readE[SerializableGameState](s).right
      messages <- ser.messages.deserialize(fs).right
      warehouses <- Utils.lift(ser.warehouses.map(_.deserialize(fs))).right
    } yield {
      val result = new GameState()
      result.setMessages(messages.file, messages.content)
      warehouses.foreach(fc => result.add(fc.file, fc.content))
      result
    }
  }

  def save(gameState: GameState) : Either[IOError, String] = {
    writeE[SerializableGameState](gameState.serialize())
  }

  def delete(fileContent: FileContent[_]): Option[IOError] = {
    fileContent.file.parent.deleteFile(fileContent.file.name)
  }
}

case class SerializableGameState(messages: SerializedContent[Messages], warehouses: Seq[SerializedContent[Warehouse]])

class GameState() {
  private var messages: FileContent[Messages] = null
  private val warehouses = new ArrayBuffer[FileContent[Warehouse]]

  def setMessages(file: VirtualFile, messages: Messages): Unit = {
    this.messages = FileContent(file, messages)
  }

  def add(file: VirtualFile, warehouse: Warehouse): Unit = {
    warehouses += FileContent(file, warehouse)
  }

  def serialize() = SerializableGameState(messages.serialize(), warehouses.map(_.serialize()))

  def delete() : Option[IOError] = {
    messages.file.parent.deleteFile(messages.file.name)
      .orElse(
        Utils.mapFirstSome(warehouses, GameState.delete)
      )
  }

}
