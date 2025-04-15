package org.enricobn.consolegame

import org.enricobn.consolegame.content.Messages
import org.enricobn.shell.impl.VirtualShell
import org.enricobn.terminal.Terminal

import scala.compiletime.uninitialized

class FakeTerminal extends Terminal {
  private var shell: VirtualShell = uninitialized
  private var text: String = ""

  override def onInput(subscriber: String => Unit): Unit = {}

  override def removeOnInput(subscriber: String => Unit) : Unit = {}

  override def removeOnInputs(): Unit = {}

  override def add(text: String): Unit = {
    this.text += text
  }

  override def flush(): Unit = {
    Messages.addMessage(shell, shell.authentication.user + ":" + text)
    text = ""
  }

  def setShell(shell: VirtualShell): Unit = {
    this.shell = shell
  }
}
