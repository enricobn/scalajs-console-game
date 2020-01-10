package org.enricobn.buyandsell.commands

import org.enricobn.buyandsell.content.Market
import org.enricobn.shell.impl.VirtualShell
import org.enricobn.shell.{ShellInput, ShellOutput, VirtualCommand, VirtualProcess}
import org.enricobn.vfs.IOError

object MainLoopCommand {
  val name = "mainLoop"
}

// TODO it's an example
class MainLoopCommand extends VirtualCommand {

  override def name: String = MainLoopCommand.name

  override def run(shell: VirtualShell, shellInput: ShellInput, shellOutput: ShellOutput, args: String*): Either[IOError, VirtualProcess] =
    Right(new VirtualProcess() {
      override def update(): Unit = {
        Market.plus(shell, "test", 0.1)
      }

      override def running: Boolean = true
    })

  override def completion(line: String, shell: VirtualShell): Seq[String] = Seq.empty
}
