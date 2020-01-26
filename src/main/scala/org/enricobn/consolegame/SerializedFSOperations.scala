package org.enricobn.consolegame

import org.enricobn.shell.impl.VirtualShell
import org.enricobn.vfs.IOError._
import org.enricobn.vfs._
import org.enricobn.vfs.utils.Utils

object SerializedFSOperations {

  /**
    * Loads the given serializedFS using the given shell.
    */
  def load(shell: VirtualShell, serializers: Map[String, Serializer], serializedFS: SerializedFS)
          (implicit authentication: Authentication) : Either[IOError, Unit] =
    for {
      // I sort them so I create them in order
      _ <- Utils.lift(serializedFS.folders.sortBy(_.path).map(serializedFolder => {
        for {
          folder <- mkdir(shell, serializedFolder.path).right
          _ <- folder.chown(serializedFolder.owner).right
          _ <- folder.chgrp(serializedFolder.group).right
          result <- folder.chmod(serializedFolder.permissions).right
        } yield {
          result
        }
      })).right
      serializedAndSerializers <- Utils.liftTuple(serializedFS.files.map(serializedFile => {
        val serializerE = serializers.get(serializedFile.serializerName)
          .toRight(IOError(s"Cannot find serializer with name=${serializedFile.serializerName}"))
        (serializedFile, serializerE)
      })).right
      serializedAndSerializerAndContent <- {
        val ssc = serializedAndSerializers.map { case (serializedFile, serializer) =>
          val path = VirtualPath(serializedFile.path)

          for {
            parentFolder <- shell.toFolder(path.parentFragments.get.path).right
            foundFile <- parentFolder.findFile(path.name).right
            file <- if (foundFile.isDefined) {
                      Right(foundFile.get).right
                    } else {
                      parentFolder.touch(path.name).right
                    }
            _ <- file.chown(serializedFile.owner).right
            _ <- file.chgrp(serializedFile.group).right
            _ <- file.chmod(serializedFile.permissions).right
          } yield fileSerializerDeSerialized(serializedFile, serializer)
        }

        val result = Utils.lift(ssc) match {
          case Right(r) => Utils.liftTuple(r)
          case Left(e) => Left(e)
        }

        result.right
      }
      contentFiles <- Utils.liftTuple(serializedAndSerializerAndContent.map {case ((serializedFile, _), content) =>
        (content, shell.toFile(serializedFile.path))
      }).right
      _ <- Utils.lift(contentFiles.map { case (content, file) => file.setContent(content) }).right
    } yield ()

  /**
    * Builds a SerializedFS.
    * Only files with serializable content (exists in the serializers map) are serialized.
    * Is it correct?
    * Would be better to raise an error? But in that case I must ignore the commands, so I must mark them as
    * non serializable in some way.
    */
  def build(files: Set[VirtualFile], folders: List[VirtualFolder], serializers: Map[String, Serializer])
           (implicit authentication: Authentication): Either[IOError, SerializedFS] =
    for {
      fileContents <- Utils.liftTuple(
        files.map(file => (file, file.getContent))
      ).right
      fileContentSerializers <- Right(Utils.allSome(fileContents.map { fileContentSerializer(_, serializers)})).right
      serializedContents <- Utils.liftTuple(
        fileContentSerializers.map { case ((file, content), serializer) => ((file, serializer), serializer.serialize(content))
        }).right
      files <- Right(serializedContents.map { case ((file, serializer), ser) =>
        SerializedFile(file.path, file.owner, file.group, file.permissions.octal, serializer.name, ser)
      }).right
      folders <- Right(folders.map(folder => SerializedFolder(folder.path, folder.owner, folder.group, folder.permissions.octal))).right
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

  private def mkdir(shell:VirtualShell, path: String)(implicit authentication: Authentication) : Either[IOError, VirtualFolder] = {
    val virtualPath = VirtualPath(path)

    val parentPath = virtualPath.parentFragments.get.path

    shell.toFolder(parentPath) match {
      case Left(error) => s"Cannot make directory $path, cannot find $parentPath : ${error.message}".ioErrorE
      case Right(parent) =>
        parent.findFolder(virtualPath.name) match {
          case Right(Some(folder)) => Right(folder)
          case Right(None) => parent.mkdir(virtualPath.name)
          case Left(error) => Left(error)
        }

    }
  }

}
