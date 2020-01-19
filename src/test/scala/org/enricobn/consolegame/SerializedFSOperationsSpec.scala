package org.enricobn.consolegame

import org.enricobn.SpecWithShell
import org.enricobn.buyandsell.BuyAndSell
import org.enricobn.vfs.utils.Utils.RightBiasedEither

import scala.language.{existentials, reflectiveCalls}

class SerializedFSOperationsSpec extends SpecWithShell {

  private val serializers =
    (ConsoleGame.globalSerializers ++ BuyAndSell.serializers)
      .map(serializer =>
        (serializer.clazz.getName, serializer)
      ).toMap

  ignore should "deserialize" in {
    val f = fixture
    val content = "{\"userName\":\"enrico\",\"fs\":{\"folders\":[{\"path\":\"/etc\",\"owner\":\"root\",\"permissions\":775},{\"path\":\"/bin\",\"owner\":\"root\",\"permissions\":775},{\"path\":\"/home\",\"owner\":\"root\",\"permissions\":775},{\"path\":\"/usr\",\"owner\":\"root\",\"permissions\":775},{\"path\":\"/var\",\"owner\":\"root\",\"permissions\":775},{\"path\":\"/home/enrico\",\"owner\":\"enrico\",\"permissions\":775},{\"path\":\"/home/enrico/Pisa\",\"owner\":\"enrico\",\"permissions\":775},{\"path\":\"/usr/bin\",\"owner\":\"root\",\"permissions\":775},{\"path\":\"/var/log\",\"owner\":\"root\",\"permissions\":775}],\"files\":[{\"path\":\"/home/enrico/Pisa/warehouse\",\"owner\":\"enrico\",\"permissions\":664,\"serializerName\":\"org.enricobn.buyandsell.content.Warehouse\",\"ser\":\"{\\\"goods\\\":{\\\"gold\\\":2,\\\"silver\\\":8,\\\"bronze\\\":20}}\"},{\"path\":\"/home/enrico/.history\",\"owner\":\"enrico\",\"permissions\":664,\"serializerName\":\"org.enricobn.shell.impl.StringList\",\"ser\":\"{\\\"value\\\":[\\\"ls\\\",\\\"sell Pisa/warehouse silver 2\\\"]}\"},{\"path\":\"/var/log/messages.log\",\"owner\":\"enrico\",\"permissions\":664,\"serializerName\":\"org.enricobn.consolegame.content.Messages\",\"ser\":\"{\\\"messages\\\":[\\\"sell 2 of silver\\\"]}\"},{\"path\":\"/etc/profile\",\"owner\":\"root\",\"permissions\":664,\"serializerName\":\"org.enricobn.shell.impl.StringMap\",\"ser\":\"{\\\"value\\\":{\\\"PATH\\\":\\\"/bin:/usr/bin\\\"}}\"},{\"path\":\"/home/enrico/gamestats\",\"owner\":\"enrico\",\"permissions\":664,\"serializerName\":\"org.enricobn.buyandsell.content.GameStatistics\",\"ser\":\"{\\\"money\\\":10000}\"},{\"path\":\"/home/enrico/Pisa/city\",\"owner\":\"enrico\",\"permissions\":664,\"serializerName\":\"org.enricobn.buyandsell.content.City\",\"ser\":\"{\\\"name\\\":\\\"Pisa\\\",\\\"statistics\\\":{\\\"population\\\":100,\\\"employed\\\":0}}\"},{\"path\":\"/home/enrico/.profile\",\"owner\":\"enrico\",\"permissions\":664,\"serializerName\":\"org.enricobn.shell.impl.StringMap\",\"ser\":\"{}\"},{\"path\":\"/home/enrico/market\",\"owner\":\"enrico\",\"permissions\":664,\"serializerName\":\"org.enricobn.buyandsell.content.Market\",\"ser\":\"{\\\"prices\\\":{\\\"gold\\\":1000,\\\"silver\\\":500,\\\"bronze\\\":100}}\"}]}}"

    (for {
      serializedGame <- UpickleUtils.readE[SerializedGame](content)
      _ <- SerializedFSOperations.load(f.shell, serializers, serializedGame.fs)(f.rootAuthentication)
      home <- f.shell.homeFolder
      gamestats <- home.findFile("gamestats")(f.rootAuthentication)
    } yield {
      assert(serializedGame.userName == "enrico")
      assert(gamestats.isDefined)
    }).left.foreach{ e => fail(e.message) }
  }

}

