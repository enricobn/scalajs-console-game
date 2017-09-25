package org.enricobn.buyandsell

import org.enricobn.buyandsell.content.Warehouse
import org.enricobn.consolegame.GameState.readE
import org.enricobn.consolegame.{FileContent, GameState, GameStateFactory, SerializedContent}
import org.enricobn.vfs.{IOError, VirtualFS, VirtualFile}
import org.enricobn.vfs.utils.Utils

import scala.collection.mutable.ArrayBuffer

case class BuyAndSellSerializableGameState(warehouses: Seq[SerializedContent[Warehouse]])

class BuyAndSellGameState() extends GameState[BuyAndSellSerializableGameState] {
  private val warehouses = new ArrayBuffer[FileContent[Warehouse]]

  def add(file: VirtualFile, warehouse: Warehouse): Unit = {
    warehouses += FileContent(file, warehouse)
  }

  override def toSerializable = BuyAndSellSerializableGameState(warehouses.map(_.serialize()))

  override def files: Seq[VirtualFile] = warehouses.map(_.file)

}

object BuyAndSellGameStateFactory extends GameStateFactory[BuyAndSellGameState, BuyAndSellSerializableGameState] {

  override def deserialize(ser: BuyAndSellSerializableGameState, fs: VirtualFS): Either[IOError, BuyAndSellGameState] =
    for {
      warehouses <- Utils.lift(ser.warehouses.map(_.deserialize(fs))).right
    } yield {
      val result = new BuyAndSellGameState()
      warehouses.foreach(fc => result.add(fc.file, fc.content))
      result
    }

  override def create() = new BuyAndSellGameState()

  override def serialize(gameState: BuyAndSellGameState): Either[IOError, String] =
    GameState.writeE(gameState.toSerializable)

}
