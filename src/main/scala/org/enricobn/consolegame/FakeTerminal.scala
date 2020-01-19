package org.enricobn.consolegame

import org.enricobn.consolegame.content.Messages
import org.enricobn.shell.impl.VirtualShell
import org.enricobn.terminal.Terminal

import scala.collection.mutable

class FakeTerminal extends Terminal {
  private var shell: VirtualShell = _
  private var text: String = ""

  override def onInput(subscriber: mutable.Subscriber[String, mutable.Publisher[String]]) {}

  override def removeOnInput(subscriber: mutable.Subscriber[String, mutable.Publisher[String]]) {}

  override def removeOnInputs() {}

  override def add(text: String) {
    this.text += text
  }

  override def flush() {
    Messages.addMessage(shell, shell.authentication.user + ":" + text)
    text = ""
  }

  def setShell(shell: VirtualShell) {
    this.shell = shell
  }
}
