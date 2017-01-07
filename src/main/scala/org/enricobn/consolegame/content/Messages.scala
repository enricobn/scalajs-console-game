package org.enricobn.consolegame.content

import org.enricobn.consolegame.ContentSerializer
import org.enricobn.terminal.StringPub
import org.enricobn.vfs.IOError

import scala.collection.mutable.ListBuffer

/**
  * Created by enrico on 12/25/16.
  */

case class SerializableMessages(messages: List[String]) {
  def deserialize() = new Messages(Some(this))
}

object MessagesSerializer extends ContentSerializer[Messages] {
  override def toString(content: Messages): Either[IOError, String] = {
    writeE(content.serialize())
  }

  override def fromString(s: String): Either[IOError, Messages] = {
    readE[SerializableMessages](s).right.map(_.deserialize())
  }

  override val clazz: Class[Messages] = classOf[Messages]
}

class Messages(serialized: Option[SerializableMessages] = None) {
  private val _messages = new ListBuffer[String]()
  private val publisher = new StringPub()

  serialized.foreach(_messages ++= _.messages)

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

  def serialize() : SerializableMessages = SerializableMessages(_messages.toList)
}
