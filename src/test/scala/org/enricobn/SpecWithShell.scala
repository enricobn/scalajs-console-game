package org.enricobn

import org.enricobn.shell.impl.{UnixLikeInMemoryFS, UnixLikeVirtualShell, VirtualShell}
import org.enricobn.terminal.Terminal
import org.enricobn.vfs.Authentication
import org.enricobn.vfs.inmemory.InMemoryFS
import org.scalamock.matchers.Matchers
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec

abstract class SpecWithShell extends AnyFlatSpec with MockFactory with Matchers {

  protected def fixture: Fixture = {
    val term = mock[Terminal]
    val rootPassword = "root"

    val fs = UnixLikeInMemoryFS(InMemoryFS(rootPassword).toOption.get, rootPassword).toOption.get

    val vum = fs.vum

    val _rootAuthentication = vum.logRoot(rootPassword).toOption.get

    vum.addUser("enrico", "enrico", "enrico")(_rootAuthentication)

    val virtualShell = UnixLikeVirtualShell(fs, term, fs.root, _rootAuthentication)

    Fixture(virtualShell, _rootAuthentication)
  }

}

protected case class Fixture(shell: VirtualShell, rootAuthentication: Authentication)
