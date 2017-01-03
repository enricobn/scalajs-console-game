package org.enricobn.consolegame.content

import org.enricobn.consolegame.ContentSerializer
import org.enricobn.terminal.StringPub

import scala.collection.mutable.ListBuffer
import upickle.default._

/**
  * Created by enrico on 12/25/16.
  */

case class SerializableMessages(messages: List[String])

object MessagesSerializer extends ContentSerializer[Messages] {
  override def toString(content: Messages): String = {
    val ser = new SerializableMessages(content.messages)
    write(ser)
  }

  override def fromString(s: String): Messages = {
    val ser = read[SerializableMessages](s)
    val messages = new Messages()
    ser.messages.foreach(messages.add)
    messages
  }

  override val clazz: Class[Messages] = classOf[Messages]
}

class Messages {
  private val _messages = new ListBuffer[String]()
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
