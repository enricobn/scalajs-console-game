package org.enricobn.consolegame

import org.enricobn.consolegame.BrowserConsoleGame._
import org.enricobn.terminal.{CanvasInputHandler, CanvasTextScreen, JSLoggerImpl, TerminalImpl}
import org.scalajs.dom
import org.scalajs.dom.FileReader
import org.scalajs.dom.html.{Anchor, Button, Canvas, Input}
import org.scalajs.dom.raw.{Event, File, MouseEvent, UIEvent}

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

  private[consolegame] def readGame(input: Input)(evt: Event): Unit = {
    //Retrieve the first (and only!) File from the FileList object
    val f = evt.target.asInstanceOf[Input].files(0)

    if (f != null) {
      val r = new FileReader()
      r.onload = fileReaderOnLoad(f, r) _
      r.readAsText(f)
    }
  }

  private def fileReaderOnLoad(f: File, r: FileReader)(e: UIEvent) {
    loadGame(f.name, r.result.toString)
  }

  override def executeLater(runnable: () => Unit): Unit = {
    dom.window.setTimeout(runnable, 100)
  }

  override def showError(message: String): Unit = dom.window.alert(message)

}
