package org.enricobn.buyandsell.commands

import org.enricobn.buyandsell.content._
import org.enricobn.shell.impl.{StringArgument, VirtualCommandAbstract, VirtualShell}
import org.enricobn.shell.{ShellInput, ShellOutput, VirtualProcess}
import org.enricobn.vfs.{Authentication, IOError}

object CreateCityCommand extends VirtualCommandAbstract("createcity", StringArgument("name", required = true)) {

  override def runParsed(shell: VirtualShell, shellInput: ShellInput, shellOutput: ShellOutput, args: Seq[Any])
                        (implicit authentication: Authentication)
  : Either[IOError, VirtualProcess] = {
    import org.enricobn.vfs.utils.Utils.RightBiasedEither

    val cityName = args.head.asInstanceOf[String]

    val city = City(cityName, Statistics(population = 0, employed = 0))

    for {
      warehouse <- Warehouse(List())
        .change(GoodEnum.gold, 5)
        .flatMap(_.change(GoodEnum.silver, 10))
        .flatMap(_.change(GoodEnum.bronze, 20))
        .flatMap(_.setPrice(GoodEnum.gold, Price(1000)))
        .flatMap(_.setPrice(GoodEnum.silver, Price(300)))
        .flatMap(_.setPrice(GoodEnum.bronze, Price(50)))
      home <- shell.homeFolder
      cityFolder <- home.mkdir(cityName)
      _ <- cityFolder.createFile("city", city)
      _ <- cityFolder.createFile("warehouse", warehouse)
      _ <- Right(shell.currentFolder = cityFolder)
    } yield new VirtualProcess()
  }

  override def completion(line: String, shell: VirtualShell): Seq[String] = Seq.empty

}

