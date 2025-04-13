package org.enricobn.consolegame

import org.enricobn.shell.impl.VirtualShell
import org.enricobn.vfs.*
import org.enricobn.vfs.utils.Utils

object SerializedFSOperations {

  /**
    * Loads the given serializedFS using the given shell.
    */
  def load(shell: VirtualShell, serializers: Map[String, Serializer], serializedFS: SerializedFS)
          (implicit authentication: Authentication): Either[IOError, Unit] =
    for {
      // I sort them so I create them in order
      _ <- Utils.lift(serializedFS.folders.sortBy(_.path).map(serializedFolder => {
        for {
          folder <- mkdir(shell, serializedFolder.path)
          _ <- folder.chown(serializedFolder.owner)
          _ <- folder.chgrp(serializedFolder.group)
          result <- folder.chmod(serializedFolder.permissions)
        } yield {
          result
        }
      }))
      serializedAndSerializers <- Utils.liftTuple(serializedFS.files.map(serializedFile => {
        val serializerE = serializers.get(serializedFile.serializerName)
          .toRight(IOError(s"Cannot find serializer with name=${serializedFile.serializerName}"))
        (serializedFile, serializerE)
      }))
      serializedAndSerializerAndContent <- {
        val ssc = serializedAndSerializers.map { case (serializedFile, serializer) =>
          for {
            path <- VirtualPath.of(serializedFile.path)
            parentPath <- path.parentOrError
            parentFolder <- parentPath.toFolder(shell.currentFolder)
            foundFile <- parentFolder.findFile(path.name)
            file <- if (foundFile.isDefined) {
              Right(foundFile.get)
            } else {
              parentFolder.touch(path.name)
            }
            _ <- file.chown(serializedFile.owner)
            _ <- file.chgrp(serializedFile.group)
            _ <- file.chmod(serializedFile.permissions)
          } yield fileSerializerDeSerialized(serializedFile, serializer)
        }

        val result = Utils.lift(ssc) match {
          case Right(r) => Utils.liftTuple(r)
          case Left(e) => Left(e)
        }

        result
      }
      contentFiles <- Utils.liftTuple(serializedAndSerializerAndContent.map { case ((serializedFile, _), content) =>
        (content, shell.toFile(serializedFile.path))
      })
      _ <- Utils.lift(contentFiles.map { case (content, file) => file.setContent(content) })
    } yield ()

  /**
    * Builds a SerializedFS.
    * Only files with serializable content (exists in the serializers map) are serialized.
    * Is it correct?
    * Would be better to raise an error? But in that case I must ignore the commands, so I must mark them as
    * non serializable in some way.
    */
  def build(files: Seq[VirtualFile], folders: List[VirtualFolder], serializers: Map[String, Serializer])
           (implicit authentication: Authentication): Either[IOError, SerializedFS] =
    for {
      fileContents <- Utils.liftTuple(
        files.map(file => (file, file.getContent))
      )
      fileContentSerializers <- Right(Utils.allSome(fileContents.map {
        fileContentSerializer(_, serializers)
      }))
      serializedContents <- Utils.liftTuple(
        fileContentSerializers.map { case ((file, content), serializer) => ((file, serializer), serializer.serialize(content))
        })
      files <- Right(serializedContents.map { case ((file, serializer), ser) =>
        SerializedFile(file.path, file.owner, file.group, file.permissions.octal, serializer.name, ser)
      })
      folders <- Right(folders.map(folder => SerializedFolder(folder.path, folder.owner, folder.group, folder.permissions.octal)))
    } yield {
      SerializedFS(folders, files)
    }

  private def fileSerializerDeSerialized(serializedFile: SerializedFile, serializer: Serializer):
  ((SerializedFile, Serializer), Either[IOError, AnyRef]) =
    ((serializedFile, serializer), serializer.deserialize(serializedFile.ser))

  private def fileContentSerializer(fileContent: (VirtualFile, AnyRef), serializers: Map[String, Serializer]) = {
    val (file, content) = fileContent
    val maybeSerializer = serializers.get(content.getClass.getName)

    if (maybeSerializer.isEmpty) {
      println(s"Cannot serialize ${content.getClass.getName}")
    }

    ((file, content), maybeSerializer)
  }

  private def mkdir(shell: VirtualShell, path: String)(implicit authentication: Authentication): Either[IOError, VirtualFolder] = {
    for {
      virtualPath <- VirtualPath.of(path)
      parentPath <- virtualPath.parentOrError
      parent <- shell.toFolder(parentPath.toString)
      result <- parent.findFolder(virtualPath.name) match {
        case Right(Some(folder)) => Right(folder)
        case Right(None) => parent.mkdir(virtualPath.name)
        case Left(error) => Left(error)
      }
    } yield result
  }

}
