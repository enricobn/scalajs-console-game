package org.enricobn

import org.enricobn.shell.impl.{UnixLikeInMemoryFS, UnixLikeVirtualShell, VirtualShell}
import org.enricobn.terminal.Terminal
import org.enricobn.vfs.Authentication
import org.enricobn.vfs.inmemory.InMemoryFS
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

abstract class SpecWithShell extends FlatSpec with MockFactory with Matchers {

  protected def fixture: Fixture = {
    val term = mock[Terminal]
    val rootPassword = "root"

    val fs = UnixLikeInMemoryFS(InMemoryFS(rootPassword).right.get, rootPassword).right.get

    val vum = fs.vum

    val _rootAuthentication = vum.logRoot(rootPassword).right.get

    vum.addUser("enrico", "enrico")(_rootAuthentication)

    val virtualShell = UnixLikeVirtualShell(fs, term, fs.root, _rootAuthentication)

    Fixture(virtualShell, _rootAuthentication)
  }

}

protected case class Fixture(shell: VirtualShell, rootAuthentication: Authentication)
