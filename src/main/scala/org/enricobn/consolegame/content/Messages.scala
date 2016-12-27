package org.enricobn.consolegame.content

import org.enricobn.terminal.StringPub

import scala.collection.mutable.ListBuffer

/**
  * Created by enrico on 12/25/16.
  */
class Messages {
  private val messages = new ListBuffer[String]()
  private val publisher = new StringPub()

  def add(message: String): Unit = {
    messages += message
    publisher.publish(message)
  }

  def subscribe(subscriber: StringPub#Sub): Unit = {
    publisher.subscribe(subscriber)
  }

  def removeSubscription(subscriber: StringPub#Sub): Unit = {
    publisher.removeSubscription(subscriber)
  }

  override def toString: String = messages.mkString("\n")
}
