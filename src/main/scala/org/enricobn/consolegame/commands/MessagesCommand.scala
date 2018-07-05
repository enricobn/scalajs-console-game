package org.enricobn.consolegame.commands

import org.enricobn.consolegame.content.Messages
import org.enricobn.shell._
import org.enricobn.shell.impl._
import org.enricobn.vfs._

import scala.collection.mutable

/**
  * Created by enrico on 12/17/16.
  */
object MessagesCommand {
  val NAME = "messages"
}

class MessagesCommand() extends VirtualCommand {
  import MessagesCommand._

  override def name: String = NAME

  override def run(shell: VirtualShell, shellInput: ShellInput, shellOutput: ShellOutput, args: String*): Either[IOError, VirtualProcess] = {
    val messagesSubscriber = new VirtualFSNotifierPub#Sub {

      override def notify(pub: mutable.Publisher[Unit], event: Unit): Unit = {
        // TODO error
        for {
          messages <- Messages.getMessages(shell).right
        } yield {
          shellOutput.write(messages.messages.last + "\n")
          shellOutput.flush()
        }
      }

    }

    var _running = true

    for {
      messagesFile <- Messages.getMessagesFile(shell).right
    } yield {
      shell.fs.notifier.addWatch(messagesFile, messagesSubscriber)

      shellInput.subscribe(in => {
        // Ctrl-C
        if (in == 3.toChar.toString) {
          _running = false
          shell.fs.notifier.removeWatch(messagesFile, messagesSubscriber)
        }
      })

      new VirtualProcess() {
        override def running: Boolean = _running
      }
    }

  }

  override def completion(line: String, shell: VirtualShell): Seq[String] = {
    Seq.empty
  }

}
