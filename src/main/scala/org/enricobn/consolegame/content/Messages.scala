package org.enricobn.consolegame.content

import org.enricobn.consolegame.UpickleUtils
import org.enricobn.shell.impl.VirtualShell
import org.enricobn.terminal.StringPub
import org.enricobn.vfs.{Authentication, IOError}

/**
  * Created by enrico on 12/25/16.
  */
object Messages {
  val messagesPath = "/var/log/messages.log"

  def getMessages(shell: VirtualShell) : Either[IOError, Messages] = {
    implicit val authentication: Authentication = shell.authentication

    for {
      messagesFile <- shell.toFile(messagesPath).right
      messages <- messagesFile.contentAs(classOf[Messages]).right
    } yield messages
  }

  def addMessage(shell: VirtualShell, message: String) : Option[IOError] = {
    implicit val authentication: Authentication = shell.authentication

    val runAddMessage = for {
      messagesFile <- shell.toFile(messagesPath).right
      messages <- messagesFile.contentAs(classOf[Messages]).right
      newMessages <- Right(messages.add(message)).right
      result <- messagesFile.setContent(newMessages).toLeft(()).right
    } yield result

    runAddMessage.left.toOption
  }
}

case class Messages(messages: Seq[String]) {
  private val publisher = new StringPub()

  def add(message: String): Messages = {
    publisher.publish(message)
    Messages(messages :+ message)
  }

  def subscribe(subscriber: StringPub#Sub): Unit = {
    publisher.subscribe(subscriber)
  }

  def removeSubscription(subscriber: StringPub#Sub): Unit = {
    publisher.removeSubscription(subscriber)
  }

  override def toString: String = messages.mkString("\n")
}

object MessagesSerializer extends SimpleSerializer(classOf[Messages]) {

  override def deserialize(ser: String): Either[IOError, Messages] = UpickleUtils.readE[Messages](ser)

  override def serializeIt(content: Messages): Either[IOError, String] = UpickleUtils.writeE(content)

}