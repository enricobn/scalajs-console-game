package org.enricobn.buyandsell.content

import org.enricobn.buyandsell.commands.BuyCommand
import org.enricobn.shell.impl.{VirtualShell, VirtualShellContextImpl, VirtualShellImpl}
import org.enricobn.shell.{Completion, VirtualCommand}
import org.enricobn.terminal.Terminal
import org.enricobn.vfs.impl.{VirtualSecurityManagerImpl, VirtualUsersManagerFileImpl}
import org.enricobn.vfs.inmemory.InMemoryFS
import org.enricobn.vfs.{Authentication, VirtualFolder}
import org.scalamock.matchers.Matchers
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec

import scala.reflect.Selectable.reflectiveSelectable

class BuyCommandSpec extends AnyFlatSpec with MockFactory with Matchers {

  private def fixture: Object {val command: VirtualCommand; val guestFolder: VirtualFolder; val shell: VirtualShell} = {
    val fs = InMemoryFS(
      {
        VirtualUsersManagerFileImpl(_, "root").toOption.get
      },
      { (_, vum) => new VirtualSecurityManagerImpl(vum) })

    implicit val authentication: Authentication = fs.vum.logRoot("root").toOption.get

    val _ = fs.root.mkdir("bin").toOption.get
    val home = fs.root.findFolder("home").toOption.get.get
    val _guest = home.mkdir("guest").toOption.get

    new {
      val command: VirtualCommand = BuyCommand
      val guestFolder: VirtualFolder = _guest
      val shell: VirtualShell = new VirtualShellImpl(fs, stub[Terminal], fs.vum, fs.vsm, new VirtualShellContextImpl(), guestFolder,
        authentication)
    }
  }

  "completion of 'buy ../user1 f'" should "return ../user1/city1" in {
    val f = fixture

    implicit val authentication: Authentication = f.shell.authentication

    val errorOrUnit = for {
      usr1 <- f.guestFolder.mkdir("user1")
      city1 <- usr1.mkdir("city1")
      usr2 <- f.guestFolder.mkdir("user2")
      city2 <- usr2.mkdir("city2")
      file <- city1.touch("file")
      file1 <- city2.touch("file1")
      _ <- file.setContent(Warehouse(List()))
      _ <- file1.setContent(Warehouse(List()))
    } yield {
      f.shell.currentFolder = usr1
    }

    errorOrUnit.left.foreach(e => fail(e.message))

    val result = f.command.completion("buy ../user1", f.shell)

    assert(result == List(Completion("../user1/city1/", "city1/")))
  }
}