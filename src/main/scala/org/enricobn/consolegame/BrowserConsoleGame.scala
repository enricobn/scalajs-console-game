package org.enricobn.consolegame

import org.enricobn.consolegame.BrowserConsoleGame.*
import org.enricobn.shell.impl.RequestAnimationFrameScheduler
import org.enricobn.terminal.*
import org.scalajs.dom
import org.scalajs.dom.html.{Anchor, Button, Canvas, Input}
import org.scalajs.dom.{Blob, FileReader}

import scala.scalajs.js

object BrowserConsoleGame {
  val logger = new JSLoggerImpl()
  val typeWriterSound = "typewriter-key-1.wav"

  val colors = new TermColors()

  colors.set(ColorEnum.blue, "#6060ff")
  colors.set(ColorEnum.green, "#00ee00")
  colors.set(ColorEnum.white, "#d0d0d0")
  colors.set(ColorEnum.black, "#131926")
}

abstract class BrowserConsoleGame(mainCanvasID: String, messagesCanvasID: String, newGameID: String, loadGameID: String,
                                  saveGameID: String)
  extends ConsoleGame(new TerminalImpl(new CanvasTextScreen(mainCanvasID, logger), new CanvasInputHandler(mainCanvasID),
    logger, typeWriterSound, BrowserConsoleGame.colors),
    new TerminalImpl(new CanvasTextScreen(messagesCanvasID, logger), new CanvasInputHandler(messagesCanvasID), logger, typeWriterSound, BrowserConsoleGame.colors),
    logger, new RequestAnimationFrameScheduler()) {

  private val newGameButton = dom.document.getElementById(newGameID).asInstanceOf[Button]
  private val loadGame = dom.document.getElementById(loadGameID).asInstanceOf[Input]
  private val saveGameAnchor = dom.document.getElementById(saveGameID).asInstanceOf[Anchor]
  private val mainCanvas = dom.document.getElementById(mainCanvasID).asInstanceOf[Canvas]

  newGameButton.onclick = { (_: dom.MouseEvent) => {
    mainCanvas.contentEditable = "true"
    mainCanvas.focus()
    onNewGame()
  }
  }

  loadGame.addEventListener("change", readGame(loadGame) _, useCapture = false)

  saveGameAnchor.onclick = onSaveGame _

  private[consolegame] def readGame(input: Input)(evt: dom.Event): Unit = {
    //Retrieve the first (and only!) File from the FileList object
    val f = evt.target.asInstanceOf[Input].files(0)

    if (f != null) {
      val r = new FileReader()
      r.onload = fileReaderOnLoad(f, r) _
      r.readAsText(f)
    }
  }

  private def fileReaderOnLoad(f: dom.File, r: FileReader)(e: dom.ProgressEvent): Unit = {
    loadGame(f.name, r.result.toString)
  }

  private def onSaveGame(evt: dom.MouseEvent): Unit = {
    onSaveGame()
  }

  override def executeLater(runnable: () => Unit): Unit = {
    dom.window.setTimeout(runnable, 100)
  }

  override def showError(message: String): Unit = dom.window.alert(message)

  override def saveToFile(content: String, fileName: String): Unit = {
    val file = new Blob(js.Array(content), dom.BlobPropertyBag("text/plain"))
    saveGameAnchor.href = dom.URL.createObjectURL(file)
    saveGameAnchor.pathname = fileName
  }

}
