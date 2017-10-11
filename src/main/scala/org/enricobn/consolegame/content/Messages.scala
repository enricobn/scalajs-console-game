package org.enricobn.consolegame.content

import org.enricobn.consolegame.UpickleUtils
import org.enricobn.terminal.StringPub
import org.enricobn.vfs.IOError

/**
  * Created by enrico on 12/25/16.
  */
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