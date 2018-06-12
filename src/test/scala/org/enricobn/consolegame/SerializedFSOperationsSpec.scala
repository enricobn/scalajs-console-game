package org.enricobn.consolegame

import org.enricobn.buyandsell.BuyAndSell
import org.enricobn.consolegame.content.MessagesSerializer
import org.enricobn.shell.impl.{VirtualShell, VirtualShellContextImpl}
import org.enricobn.terminal.Terminal
import org.enricobn.vfs.impl.{VirtualSecurityManagerImpl, VirtualUsersManagerImpl}
import org.enricobn.vfs.inmemory.InMemoryFS
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

import scala.language.reflectiveCalls

class SerializedFSOperationsSpec extends FlatSpec with MockFactory with Matchers {

  private val serializers =
    (MessagesSerializer :: BuyAndSell.serializers)
      .map(serializer =>
        (serializer.clazz.getName, serializer)
      ).toMap

  def fixture = {
    val term = mock[Terminal]
    val rootPassword = "root"
    val vum = new VirtualUsersManagerImpl(rootPassword)
    val vsm = new VirtualSecurityManagerImpl(vum)

    val _rootAuthentication = vum.logRoot(rootPassword).right.get

    vum.addUser("enrico", "enrico")(_rootAuthentication)

    val fs = new InMemoryFS(vum, vsm)
    val context = new VirtualShellContextImpl()
    val virtualShell = new VirtualShell(term, vum, vsm, context, fs.root, _rootAuthentication)

    new {
      val shell = virtualShell
      val terminal = term
      val virtualUsersManager = vum
      val root = fs.root
      val rootAuthentication = _rootAuthentication
    }
  }

  "deserializing" should "be fine" in {
    val f = fixture
    val content = "{\"folders\":[{\"path\":\"/bin\",\"owner\":\"root\",\"permissions\":775},{\"path\":\"/var\",\"owner\":\"root\",\"permissions\":775},{\"path\":\"/usr\",\"owner\":\"root\",\"permissions\":775},{\"path\":\"/home\",\"owner\":\"root\",\"permissions\":775},{\"path\":\"/var/log\",\"owner\":\"root\",\"permissions\":775},{\"path\":\"/usr/bin\",\"owner\":\"root\",\"permissions\":775},{\"path\":\"/home/enrico\",\"owner\":\"enrico\",\"permissions\":775},{\"path\":\"/home/enrico/Pisa\",\"owner\":\"enrico\",\"permissions\":775}],\"files\":[{\"path\":\"/var/log/messages.log\",\"owner\":\"enrico\",\"permissions\":664,\"serializerName\":\"org.enricobn.consolegame.content.Messages\",\"ser\":\"{\\\"messages\\\":[\\\"sell 1 of silver\\\"]}\"},{\"path\":\"/home/enrico/gamestats\",\"owner\":\"enrico\",\"permissions\":664,\"serializerName\":\"org.enricobn.buyandsell.content.GameStatistics\",\"ser\":\"{\\\"money\\\":10000}\"},{\"path\":\"/home/enrico/Pisa/city\",\"owner\":\"enrico\",\"permissions\":664,\"serializerName\":\"org.enricobn.buyandsell.content.City\",\"ser\":\"{\\\"name\\\":\\\"Pisa\\\",\\\"statistics\\\":{\\\"population\\\":100,\\\"employed\\\":0}}\"},{\"path\":\"/home/enrico/Pisa/warehouse\",\"owner\":\"enrico\",\"permissions\":664,\"serializerName\":\"org.enricobn.buyandsell.content.Warehouse\",\"ser\":\"{\\\"goods\\\":{\\\"gold\\\":2,\\\"silver\\\":9,\\\"bronze\\\":20}}\"},{\"path\":\"/home/enrico/market\",\"owner\":\"enrico\",\"permissions\":664,\"serializerName\":\"org.enricobn.buyandsell.content.Market\",\"ser\":\"{\\\"prices\\\":{\\\"gold\\\":1000,\\\"silver\\\":500,\\\"bronze\\\":100}}\"}]}"
    val serializedFS = UpickleUtils.readE[SerializedFS](content)

    serializedFS.right.get.folders.foreach(println(_))
    serializedFS.right.get.files.foreach(println(_))

    serializedFS.left.map({ error => fail(error.message) })

    val value = SerializedFSOperations.load(f.shell, serializers, serializedFS.right.get)(f.rootAuthentication)

    value.left.map({ error => fail(error.message) })

    assert(f.shell.findFile("/home/enrico/gamestats").right.get.isDefined)

  }

}
