package org.enricobn.consolegame

import org.enricobn.shell.impl.VirtualShell
import org.enricobn.vfs.{IOError, VirtualFile, VirtualFolder, VirtualPath}
import org.enricobn.vfs.utils.Utils

object FSSerializer {

  def load(shell: VirtualShell, serializers: Map[String, Serializer], ser: String): Either[IOError, Unit] =
    for {
      serializedFS <- UpickleUtils.readE[SerializedFS](ser).right
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
        { case (content, file) => file.content = content }
      ).toLeft(()).right
    } yield {
      result
    }

  def save(files: Set[VirtualFile], folders: List[VirtualFolder], serializers: Map[Class[_], Serializer]): Either[IOError, String] =
    for {
      fileContents <- FunctionalUtils.lift(
        files.map(file => (file, file.content))
      ).right
      fileContentSerializers <- Right(FunctionalUtils.allSome(fileContents.map {case (file, content) => ((file,content), serializers.get(content.getClass))})).right
      serializedContents <- FunctionalUtils.lift(
        fileContentSerializers.map { case ((file, content), serializer) => ((file, serializer), serializer.serialize(content))
        }).right
      files <- Right(serializedContents.map { case ((file, serializer), ser) =>
        SerializedFile(file.path, file.owner, file.permissions.octal, serializer.name, ser)
      }).right
      folders <- Right(folders.map(folder => SerializedFolder(folder.path, folder.owner, folder.permissions.octal))).right
      ser <- UpickleUtils.writeE(SerializedFS(folders, files)).right
    } yield {
      ser
    }

  private def mkdir(shell:VirtualShell, path: String) : Either[IOError, VirtualFolder] = {
    val virtualPath = VirtualPath(path)

    shell.toFolder(virtualPath.parentFragments.get.path) match {
      case error@Left(_) => error
      case Right(parent) => parent.mkdir(virtualPath.name)
    }
  }

}
