package org.enricobn.consolegame

import org.enricobn.consolegame.BrowserConsoleGame._
import org.enricobn.terminal.{CanvasInputHandler, CanvasTextScreen, JSLoggerImpl, TerminalImpl}
import org.scalajs.dom
import org.scalajs.dom.html.{Anchor, Button, Canvas, Input}
import org.scalajs.dom.raw.MouseEvent

object BrowserConsoleGame {
  val logger = new JSLoggerImpl()
  val typeWriterSound = "typewriter-key-1.wav"
}

abstract class BrowserConsoleGame(mainCanvasID: String, messagesCanvasID: String, newGameID: String, loadGameID: String,
                                  saveGameID: String)
  extends ConsoleGame(new TerminalImpl(new CanvasTextScreen(mainCanvasID, logger), new CanvasInputHandler(mainCanvasID),
    logger, typeWriterSound),
    new TerminalImpl(new CanvasTextScreen(messagesCanvasID, logger), new CanvasInputHandler(messagesCanvasID), logger, typeWriterSound),
    logger) {

  private val newGameButton = dom.document.getElementById(newGameID).asInstanceOf[Button]
  private val loadGame = dom.document.getElementById(loadGameID).asInstanceOf[Input]
  private val saveGameAnchor = dom.document.getElementById(saveGameID).asInstanceOf[Anchor]
  private val mainCanvas = dom.document.getElementById(mainCanvasID).asInstanceOf[Canvas]

  newGameButton.onclick = { _ : MouseEvent => {
    mainCanvas.contentEditable = "true"
    mainCanvas.focus()
    onNewGame()
  }}
  loadGame.addEventListener("change", readGame(loadGame) _, useCapture = false)
  saveGameAnchor.onclick = onSaveGame(saveGameAnchor) _

}
