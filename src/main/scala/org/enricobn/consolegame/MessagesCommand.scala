package org.enricobn.consolegame

import org.enricobn.shell.impl._
import org.enricobn.shell._
import org.enricobn.terminal.StringPub
import org.enricobn.vfs.IOError._
import org.enricobn.vfs._

import scala.collection.mutable
import scala.collection.mutable.Stack

/**
  * Created by enrico on 12/17/16.
  */
class MessagesCommand(messages: Messages) extends VirtualCommand {

  override def getName: String = "messages"

  override def run(shell: VirtualShell, shellInput: ShellInput, shellOutput: ShellOutput, args: String*) = {
    val stack = new mutable.Stack[String]()

    val messagesSubscriber = new StringPub#Sub {
      override def notify(pub: mutable.Publisher[String], event: String): Unit = stack.push(event)
    }
    var _running = true

    messages.subscribe(messagesSubscriber)

    shellInput.subscribe(in => {
      // Ctrl-C
      if (in == 3.toChar.toString) {
        _running = false
        stack.clear()
        messages.removeSubscription(messagesSubscriber)
      }
    })

    Right({
      new RunContext() {

        override def running: Boolean = _running

        override def interactive: Boolean = true

        override def update(): Unit = {
          stack.foreach(message => {
            shellOutput.write(message + "\n")
            shellOutput.flush()
          })
          stack.clear()
        }
      }
    })
  }

  override def completion(line: String, currentFolder: VirtualFolder): Seq[String] = {
    Seq.empty
  }

}
