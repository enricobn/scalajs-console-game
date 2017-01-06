package org.enricobn.consolegame.content

import org.enricobn.consolegame.ContentSerializer
import org.enricobn.terminal.StringPub
import org.enricobn.vfs.IOError

import scala.collection.mutable.ListBuffer

/**
  * Created by enrico on 12/25/16.
  */

case class SerializableMessages(messages: List[String])

object MessagesSerializer extends ContentSerializer[Messages] {
  override def toString(content: Messages): Either[IOError, String] = {
    writeE(new SerializableMessages(content.messages))
  }

  override def fromString(s: String): Either[IOError, Messages] = {
    readE[SerializableMessages](s).right.map(m => new Messages(m.messages))
  }

  override val clazz: Class[Messages] = classOf[Messages]
}

class Messages(initialMessages: Seq[String] = Seq()) {
  private val _messages = new ListBuffer[String]() ++ initialMessages
  private val publisher = new StringPub()

  def messages = _messages.toList

  def add(message: String): Unit = {
    _messages += message
    publisher.publish(message)
  }

  def subscribe(subscriber: StringPub#Sub): Unit = {
    publisher.subscribe(subscriber)
  }

  def removeSubscription(subscriber: StringPub#Sub): Unit = {
    publisher.removeSubscription(subscriber)
  }

  override def toString: String = _messages.mkString("\n")
}
