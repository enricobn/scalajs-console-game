package org.enricobn.consolegame

import org.enricobn.vfs.IOError._
import org.enricobn.vfs.impl.VirtualAbsolutePath
import org.enricobn.vfs.{IOError, VirtualFS, VirtualFile}
import upickle.default._
import org.enricobn.vfs.utils.Utils

/**
  * Created by enrico on 12/30/16.
  */

case class SerializedContent[T <: AnyRef](path: String, owner: String, permissions: Int, content: T) {

  def deserialize(fs: VirtualFS) = {
    val vPath = VirtualAbsolutePath(path)
    for {
      parent <- vPath.parent.toRight(IOError(s"No parent found for $path")).right
      folder <- fs.root.resolveFolderOrError(parent.path, s"Cannot find folder $parent").right
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

trait GameStateFactory[T <: GameState[S], S <: AnyRef] {

  def deserialize(s: S, fs: VirtualFS) : Either[IOError, T]

  def serialize(gameState: T) : Either[IOError, String]

  def create() : T

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

  // TODO move in VirtualFile?
  def delete(file: VirtualFile): Option[IOError] = {
    file.parent.deleteFile(file.name)
  }
}

trait GameState[S <: AnyRef] {

  def files : Seq[VirtualFile]

  def delete() : Option[IOError] = Utils.mapFirstSome(files, GameState.delete)

  def toSerializable: S

}