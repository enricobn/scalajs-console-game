package org.enricobn.consolegame.content

import org.enricobn.terminal.StringPub
import upickle.Js

import scala.collection.mutable.ListBuffer

/**
  * Created by enrico on 12/25/16.
  */

object Messages {
  implicit val messagesWriter = upickle.default.Writer[Messages] {
    case t => Js.Arr(t.messages.map(v => Js.Str(v)).toArray :_*)
  }

  implicit val messages2Reader = upickle.default.Reader[Messages]{
    case a: Js.Arr =>
      val messages = new Messages()
      a.value.foreach {
        case Js.Str(s) => messages.add(s)
        case v => throw new IllegalArgumentException(a + " is not a Json String array.")
      }
      messages
  }
}

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
