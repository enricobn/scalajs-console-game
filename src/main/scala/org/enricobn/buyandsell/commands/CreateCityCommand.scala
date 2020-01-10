package org.enricobn.buyandsell.commands

import org.enricobn.buyandsell.commands.CreateCityCommand.{NAME, name}
import org.enricobn.buyandsell.content.{City, Statistics, Warehouse}
import org.enricobn.shell.impl.{StringArgument, VirtualCommandAbstract, VirtualShell}
import org.enricobn.shell.{ShellInput, ShellOutput, VirtualProcess}
import org.enricobn.vfs.{Authentication, IOError}

object CreateCityCommand {

  def name = "createcity"

  private val NAME = StringArgument("name", required = true)

}

class CreateCityCommand extends VirtualCommandAbstract(name, NAME) {

  override def runParsed(shell: VirtualShell, shellInput: ShellInput, shellOutput: ShellOutput, args: Seq[Any])
                        (implicit authentication: Authentication)
  : Either[IOError, VirtualProcess] = {
    import org.enricobn.vfs.utils.Utils.RightBiasedEither

    val cityName = args.head.asInstanceOf[String]

    val city = City(cityName, Statistics(population = 0, employed = 0))
    val warehouse = Warehouse(Map("gold" -> 2, "silver" -> 10, "bronze" -> 20))

    for {
      home <- shell.homeFolder
      cityFolder <- home.mkdir(cityName)
      _ <- cityFolder.createFile("city", city)
      _ <- cityFolder.createFile("warehouse", warehouse)
      _ <- Right(shell.currentFolder = cityFolder)
    } yield new VirtualProcess()
  }

  override def completion(line: String, shell: VirtualShell): Seq[String] = Seq.empty

}
