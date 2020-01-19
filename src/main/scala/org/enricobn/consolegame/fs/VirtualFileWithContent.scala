package org.enricobn.consolegame.fs

import org.enricobn.vfs.utils.Utils.RightBiasedEither
import org.enricobn.vfs.{Authentication, IOError, VirtualFile, VirtualFolder}

object VirtualFileWithContent {

  def getOrCreate[T <: AnyRef](clazz: Class[T], folder: VirtualFolder, name: String, createFunction: () => T)
                        (implicit authentication: Authentication) : Either[IOError, VirtualFileWithContent[T]] =
    for {
      fileO <- folder.findFile(name)
      file <- if (fileO.isDefined)
        Right(fileO.get)
      else
        folder.createFile(name, createFunction.apply().asInstanceOf[AnyRef])
    } yield new VirtualFileWithContent(clazz, file)

}

class VirtualFileWithContent[T <: AnyRef](clazz: Class[T], val file: VirtualFile) {

  def mapContent(mapFunction: T => T)(implicit authentication: Authentication): Either[IOError, Unit] =
    for {
      content <- file.contentAs(clazz)
      newContent = mapFunction.apply(content)
      _ <- file.setContent(newContent.asInstanceOf[AnyRef]).toLeft(())
    } yield ()

  def content()(implicit authentication: Authentication): Either[IOError, T] =
    for {
      content <- file.getContent
    } yield content.asInstanceOf[T]

}
