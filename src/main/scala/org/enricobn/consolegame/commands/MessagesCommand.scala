package org.enricobn.consolegame.commands

import org.enricobn.consolegame.content.Messages
import org.enricobn.shell.*
import org.enricobn.shell.ShellInput.ShellInputDescriptor
import org.enricobn.shell.impl.*
import org.enricobn.vfs.*

/**
  * Created by enrico on 12/17/16.
  */
object MessagesCommand {
  val NAME = "messages"
}

class MessagesCommand() extends VirtualCommand {
  import MessagesCommand.*

  override def name: String = NAME

  override def run(shell: VirtualShell, shellInput: ShellInput, shellOutput: ShellOutput, args: String*): Either[IOError, VirtualProcess] = {
    val messagesSubscriber : Unit => Unit = { _ =>
        // TODO error
        val r = for {
          messages <- Messages.getMessages(shell)
        } yield {
          shellOutput.write(messages.messages.last + "\n")
          shellOutput.flush()
        }
        r.getOrElse(())
    }

    var _running = true

    for {
      messagesFile <- Messages.getMessagesFile(shell)
    } yield {
      shell.fs.notifier.addWatch(messagesFile, messagesSubscriber)

      new VirtualProcess() {

        val ctrCDescriptor: ShellInputDescriptor = shellInput.subscribe(in => {
          // Ctrl-C
          if (in == 3.toChar.toString) {
            kill()
          }
        })

        override def running: Boolean = _running

        override def kill(): Unit = {
          shellInput.close(ctrCDescriptor)
          shell.fs.notifier.removeWatch(messagesFile, messagesSubscriber)
          _running = false
          super.kill()
        }
      }
    }

  }

  override def completion(line: String, shell: VirtualShell): Seq[Completion] = {
    Seq.empty
  }

}