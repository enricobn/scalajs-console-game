package org.enricobn.consolegame

import org.enricobn.SpecWithShell
import org.enricobn.buyandsell.BuyAndSell

import scala.language.{existentials, reflectiveCalls}

class SerializedFSOperationsSpec extends SpecWithShell {

  private val serializers =
    (ConsoleGame.globalSerializers ++ BuyAndSell.serializers)
      .map(serializer =>
        (serializer.clazz.getName, serializer)
      ).toMap

  "game" should "be deserializable" in {
    val f = fixture
    val content = readResource("buyandsell.json")

    (for {
      _ <- f.shell.vum.addUser("user1", "user1", "game")(f.rootAuthentication)
      _ <- f.shell.vum.addUser("user2", "user2", "game")(f.rootAuthentication)
      serializedGame <- UpickleUtils.readE[SerializedGame](content)
      _ <- SerializedFSOperations.load(f.shell, serializers, serializedGame.fs)(f.rootAuthentication)
      _ <- f.shell.login("enrico", "enrico")
      home <- f.shell.homeFolder
      gamestats <- home.findFile("gamestats")(f.rootAuthentication)
    } yield {
      assert(serializedGame.userName == "enrico")
      assert(gamestats.isDefined)
    }).left.foreach{ e => fail(e.message) }
  }

  /**
    * from https://stackoverflow.com/questions/40866662/unit-testing-scala-js-read-test-data-from-file-residing-in-test-resources
    */
  def readResource(path: String): String = {
    import scalajs.js.Dynamic.global as g
    val fs = g.require("fs")

    def readFile(name: String): String = {
      fs.readFileSync(name).toString
    }

    readFile(resourcesPath(path))
  }

  def resourcesPath(path: String): String = "src/test/resources/" + path

}

