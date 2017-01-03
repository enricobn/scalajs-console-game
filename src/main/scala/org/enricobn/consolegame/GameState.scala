package org.enricobn.consolegame

import scala.collection.mutable

import upickle.default._

/**
  * Created by enrico on 12/30/16.
  */

case class SerializedContent(path: String, className: String, content: String)

class GameStateFactory {
  private val classRegistry = new mutable.HashMap[String, ContentSerializer[AnyRef]]()

  def register[T <: AnyRef](contentSerializer: ContentSerializer[T]): Unit = {
    classRegistry.put(contentSerializer.clazz.getName, contentSerializer.asInstanceOf[ContentSerializer[AnyRef]])
  }

  def load(s: String) : GameState = {
    val ser = read[Iterable[SerializedContent]](s)
    val gameState = new GameState()
    ser.foreach(c => {
      val serializer = classRegistry(c.className)
      val deserialized = deserialize(c.content, serializer)
      gameState.add(c.path, deserialized)
    })
    gameState
  }

  def save(gameState: GameState) : String = {
    val ser = gameState.contents.map {case (path, value) =>
      val className: String = value.getClass.getName
      val serializer = classRegistry(className)
      SerializedContent(path, className, serialize(value , serializer))
    }
    write(ser)
  }

  private def serialize[T <: AnyRef](value: T, contentSerializer: ContentSerializer[T]) : String =
    contentSerializer.toString(value)

  private def deserialize[T <: AnyRef](s: String, contentSerializer: ContentSerializer[T]) : T =
    contentSerializer.fromString(s)

}

case class SerializableGameState(contents: Map[String, AnyRef])

class GameState() {
  private val _contents = new mutable.HashMap[String, AnyRef]()

  def add(path: String, content: AnyRef): Unit = {
    _contents(path) = content
  }

  def contents = _contents.toMap

}
