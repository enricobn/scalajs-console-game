package org.enricobn.buyandsell.commands

import org.enricobn.shell.impl.VirtualShell
import org.enricobn.shell._
import org.enricobn.vfs.IOError

// TODO it's an example
object MainLoopCommand extends VirtualCommand {

  override def name: String = "mainLoop"

  override def run(shell: VirtualShell, shellInput: ShellInput, shellOutput: ShellOutput, args: String*): Either[IOError, VirtualProcess] =
    Right(new VirtualProcess() {
      override def update(): Unit = {
      }

      override def running: Boolean = true
    })

  override def completion(line: String, shell: VirtualShell): Seq[Completion] = Seq.empty
}
