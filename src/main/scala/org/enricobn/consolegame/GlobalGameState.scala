package org.enricobn.consolegame

import org.enricobn.consolegame.GameState.readE
import org.enricobn.consolegame.content.Messages
import org.enricobn.vfs.{IOError, VirtualFS, VirtualFile}

case class GlobalSerializableGameState(messages: SerializedContent[Messages])

class GlobalGameState extends GameState[GlobalSerializableGameState] {
  private var messages: FileContent[Messages] = null

  def setMessages(file: VirtualFile, messages: Messages): Unit = {
    this.messages = FileContent(file, messages)
  }

  override def files = List(messages.file)

  override def toSerializable = GlobalSerializableGameState(messages.serialize())
}

object GlobalGameStateFactory extends GameStateFactory[GlobalGameState, GlobalSerializableGameState] {

  override def deserialize(ser: GlobalSerializableGameState, fs: VirtualFS): Either[IOError, GlobalGameState] =
    for {
      messages <- ser.messages.deserialize(fs).right
    } yield {
      val result = new GlobalGameState()
      result.setMessages(messages.file, messages.content)
      result
    }

  override def serialize(gameState: GlobalGameState): Either[IOError, String] =
    GameState.writeE(gameState.toSerializable)

  override def create() = new GlobalGameState()
}
