package org.enricobn.buyandsell.content

import org.enricobn.buyandsell.commands.BuyCommand
import org.enricobn.shell.impl.{VirtualShell, VirtualShellContextImpl, VirtualShellImpl}
import org.enricobn.shell.{Completion, VirtualCommand}
import org.enricobn.terminal.Terminal
import org.enricobn.vfs.impl.{VirtualSecurityManagerImpl, VirtualUsersManagerFileImpl}
import org.enricobn.vfs.inmemory.InMemoryFS
import org.enricobn.vfs.utils.Utils.RightBiasedEither
import org.enricobn.vfs.{Authentication, VirtualFolder}
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

import scala.language.reflectiveCalls

class BuyCommandSpec extends FlatSpec with MockFactory with Matchers {

  private def fixture = {
    val fs = InMemoryFS(
      {
        VirtualUsersManagerFileImpl(_, "root").right.get
      },
      { (_, vum) => new VirtualSecurityManagerImpl(vum) })

    implicit val authentication: Authentication = fs.vum.logRoot("root").right.get

    val _ = fs.root.mkdir("bin").right.get
    val home = fs.root.findFolder("home").right.get.get
    val _guest = home.mkdir("guest").right.get

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