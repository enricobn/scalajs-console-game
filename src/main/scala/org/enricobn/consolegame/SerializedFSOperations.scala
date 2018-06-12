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
      // I sort them so I crete them in order
      _ <- Utils.lift(serializedFS.folders.sortBy(_.path).map(serializedFolder => {
        for {
          // TODO the path is absolute, I must make all intermediate folders
          folder <- mkdir(shell, serializedFolder.path).right
          _ <- folder.chown(serializedFolder.owner).toLeft(()).right
          result <- folder.chmod(serializedFolder.permissions).toLeft(()).right
        } yield {
          result
        }
      })).right
      serializedAndSerializers <- FunctionalUtils.lift(serializedFS.files.map(serializedFile => {
        val serializerE = serializers.get(serializedFile.serializerName)
          .toRight(IOError(s"Cannot find serializer with name=${serializedFile.serializerName}"))
        (serializedFile, serializerE)
      })).right
      serializedAndSerializerAndContent <- FunctionalUtils.lift(serializedAndSerializers.map {case (serializedFile, serializer) =>
        // TODO errors
        val path = VirtualPath(serializedFile.path)
        val file = shell.toFolder(path.parentFragments.get.path).right.get.touch(path.name).right.get
        file.chown(serializedFile.owner)
        file.chmod(serializedFile.permissions)
        ((serializedFile, serializer), serializer.deserialize(serializedFile.ser))
      }).right
      contentFiles <- FunctionalUtils.lift(serializedAndSerializerAndContent.map {case ((serializedFile, serializer), content) =>
        (content, shell.toFile(serializedFile.path))
      }).right
      result <- Utils.mapFirstSome[(AnyRef, VirtualFile), IOError](contentFiles,
        { case (content, file) => file.setContent(content) }
      ).toLeft(()).right
    } yield {
      result
    }

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
      fileContents <- FunctionalUtils.lift(
        files.map(file => (file, file.getContent))
      ).right
      fileContentSerializers <- Right(FunctionalUtils.allSome(fileContents.map {case (file, content) => ((file,content), serializers.get(content.getClass.getName))})).right
      serializedContents <- FunctionalUtils.lift(
        fileContentSerializers.map { case ((file, content), serializer) => ((file, serializer), serializer.serialize(content))
        }).right
      files <- Right(serializedContents.map { case ((file, serializer), ser) =>
        SerializedFile(file.path, file.owner, file.permissions.octal, serializer.name, ser)
      }).right
      folders <- Right(folders.map(folder => SerializedFolder(folder.path, folder.owner, folder.permissions.octal))).right
    } yield {
      SerializedFS(folders, files)
    }

  private def mkdir(shell:VirtualShell, path: String)(implicit authentication: Authentication) : Either[IOError, VirtualFolder] = {
    val virtualPath = VirtualPath(path)

    val parentPath = virtualPath.parentFragments.get.path

    shell.toFolder(parentPath) match {
      case Left(error) => s"Cannot make directory $path, cannot find $parentPath : ${error.message}".ioErrorE
      case Right(parent) => parent.mkdir(virtualPath.name)
    }
  }

}
