package org.enricobn.consolegame.content

import org.enricobn.consolegame.UpickleUtils
import org.enricobn.shell.impl.VirtualShell
import org.enricobn.terminal.Terminal
import org.enricobn.vfs.{Authentication, IOError, VirtualFile}

/**
  * Created by enrico on 12/25/16.
  */
object Messages {
  private val messagesPath = "/var/log/messages.log"

  def getMessages(shell: VirtualShell) : Either[IOError, Messages] = {
    implicit val authentication: Authentication = shell.authentication

    for {
      messagesFile <- getMessagesFile(shell).right
      messages <- messagesFile.contentAs(classOf[Messages]).right
    } yield messages
  }

  def getMessagesFile(shell: VirtualShell) : Either[IOError, VirtualFile] = {
    implicit val authentication: Authentication = shell.authentication

    shell.toFile(messagesPath)
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

  def add(message: String): Messages = {
    val newMessages = Messages(messages :+ message)
    newMessages
  }

  override def toString: String = messages.mkString(Terminal.LF)
}

object MessagesSerializer extends SimpleSerializer(classOf[Messages]) {

  override def serializeIt(content: Messages): Either[IOError, String] = UpickleUtils.writeE(content)

  override def deserialize(ser: String): Either[IOError, Messages] = UpickleUtils.readE[Messages](ser)

}