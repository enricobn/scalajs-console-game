package org.enricobn.consolegame.content

import org.enricobn.consolegame.Serializer
import org.enricobn.terminal.StringPub
import org.enricobn.vfs.IOError
import upickle.Js

import scala.collection.mutable.ListBuffer

/**
  * Created by enrico on 12/25/16.
  */
class Messages() {
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

object MessagesSerializer extends Serializer {
  private val writer = upickle.default.Writer[Messages] {
    case t => Js.Arr(t.messages.map(v => Js.Str(v)).toArray :_*)
  }

  private val reader = upickle.default.Reader[Messages]{
    case a: Js.Arr =>
      val messages = new Messages()
      a.value.foreach {
        case Js.Str(s) => messages.add(s)
        case v => throw new IllegalArgumentException(a + " is not a Json String array.")
      }
      messages
  }

  override val name = "Messages"
  override val clazz: Class[Messages] = classOf[Messages]

  override def serialize(content: AnyRef): Either[IOError, String] =
    content match {
      case messages: Messages =>
        Right(writer.write(messages).toString())
      case _ => Left(IOError("Not an instance of Warehouse."))
    }

  override def deserialize(ser: String): Either[IOError, Messages] =
    try {
      Right(reader.read(upickle.json.read(ser)))
    } catch {
      case e: Exception => Left(IOError(e.getMessage))
    }

}
