package org.enricobn.buyandsell

import org.enricobn.consolegame.GameState.readE
import org.enricobn.consolegame.{FileContent, GameState, GameStateFactory, SerializedContent}
import org.enricobn.consolegame.content.{Messages, Warehouse}
import org.enricobn.vfs.{IOError, VirtualFS, VirtualFile}
import org.enricobn.vfs.utils.Utils

import scala.collection.mutable.ArrayBuffer

case class BuyAndSellSerializableGameState(messages: SerializedContent[Messages], warehouses: Seq[SerializedContent[Warehouse]])

class BuyAndSellGameState() extends GameState[BuyAndSellSerializableGameState] {
  private var messages: FileContent[Messages] = null
  private val warehouses = new ArrayBuffer[FileContent[Warehouse]]

  def setMessages(file: VirtualFile, messages: Messages): Unit = {
    this.messages = FileContent(file, messages)
  }

  def add(file: VirtualFile, warehouse: Warehouse): Unit = {
    warehouses += FileContent(file, warehouse)
  }

  override def toSerializable = BuyAndSellSerializableGameState(messages.serialize(), warehouses.map(_.serialize()))

  override def files: Seq[VirtualFile] = warehouses.map(_.file).:+(messages.file)

}

object BuyAndSellGameStateFactory extends GameStateFactory[BuyAndSellSerializableGameState,BuyAndSellGameState] {

  override def deserialize(s: String, fs: VirtualFS): Either[IOError, BuyAndSellGameState] =
    for {
      ser <- readE[BuyAndSellSerializableGameState](s).right
      messages <- ser.messages.deserialize(fs).right
      warehouses <- Utils.lift(ser.warehouses.map(_.deserialize(fs))).right
    } yield {
      val result = new BuyAndSellGameState()
      result.setMessages(messages.file, messages.content)
      warehouses.foreach(fc => result.add(fc.file, fc.content))
      result
    }

  override def create() = new BuyAndSellGameState()

  override def serialize(gameState: BuyAndSellGameState) =
    GameState.writeE(gameState.toSerializable)

}
