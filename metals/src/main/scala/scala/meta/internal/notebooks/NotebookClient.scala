package scala.meta.internal.notebooks

import org.eclipse.lsp4j._
import scala.meta.io.AbsolutePath
import scala.concurrent.Future
import NotebookCommands._
import scala.meta.internal.metals.MetalsLanguageClient
import scala.meta.internal.metals.MetalsEnrichments._
import com.google.gson.JsonObject
import com.google.gson.JsonPrimitive
import scala.meta.internal.metals.Buffers
import MarkdownCodelensNotebookClient._
import scala.meta.inputs.Input
import scala.meta.inputs.{Position => MetalsPosition}
import java.net.URI

trait NotebookClient {

  def codeLenses(path: AbsolutePath): Seq[CodeLens]

  def executeCommand(
      id: String,
      args: Seq[Object],
      notebookProvider: NotebookProvider
  ): Future[_]

}

class MarkdownCodelensNotebookClient(
    languageClient: MetalsLanguageClient,
    buffers: Buffers
) extends NotebookClient {

  override def codeLenses(path: AbsolutePath): Seq[CodeLens] = {
    val cells = cellsInText(path.toInputFromBuffers(buffers))
    val insertNewCellLenses =
      (startOfFileRange ::
        cells.map(cell => rangePositionAfter(cell.range)))
        .map(inserNewCellCodeLens(path.toURI, _))

    val runCellLenses = cells.map(runCellCodeLens(path.toURI, _))

    insertNewCellLenses ++ runCellLenses
  }

  override def executeCommand(
      id: String,
      args: Seq[Object],
      provider: NotebookProvider
  ): Future[Any] = id match {
    case InsertNewCell() =>
      val path = args(0).asInstanceOf[JsonPrimitive].getAsString.toAbsolutePath
      val range = args(1).asInstanceOf[JsonObject].toRange
      applyEdit(path, range, newCell()) //.map(_ => ())
    case RunCell() =>
      val path = args(0).asInstanceOf[JsonPrimitive].getAsString.toAbsolutePath
      val range = args(1).asInstanceOf[JsonObject].toRange
      val code = args(2).asInstanceOf[JsonPrimitive].getAsString
      val result = provider.execute(path, code)
      scribe.info(result.toString())
      applyEdit(path, range, result.asSuccess.get.data.toString()) //.map(_ => ())
  }

  private def startOfFileRange =
    new Range(new Position(0, 0), new Position(0, 0))

  private def cellsInText(source: Input): List[CellInText] = {
    val cellPattern = """#([^#>]*)([^#]*)""".r
    val matches = cellPattern.findAllMatchIn(source.text).toList
    matches.map(m =>
      new CellInText(
        MetalsPosition.Range(source, m.start, m.end).toLSP,
        m.toString,
        m.group(1)
      )
    )
  }

  private def newCell(withSample: Boolean = false): String =
    s"""|#
        |${if (withSample) """println("Hello, Notebook!")""" else ""}
        |""".stripMargin

  def inserNewCellCodeLens(pathUri: URI, range: Range): CodeLens =
    new CodeLens(
      range,
      InsertNewCell.toLSP(List(pathUri.toString, range)),
      null
    )

  def runCellCodeLens(pathUri: URI, cell: CellInText): CodeLens = {
    new CodeLens(
      cell.range,
      RunCell.toLSP(List(pathUri.toString, cell.range, cell.code)),
      null
    )
  }

  private def applyEdit(
      path: AbsolutePath,
      range: Range,
      editText: String
  ): Future[_] = {
    val params = new ApplyWorkspaceEditParams(
      new WorkspaceEdit(
        Map(path.toURI.toString -> (List(new TextEdit(range, editText)).asJava)).asJava
      )
    )
    languageClient.applyEdit(params).asScala
  }

  private def rangePositionAfter(range: Range) = {
    val position =
      new Position(range.getEnd().getLine(), range.getEnd().getCharacter() + 1)
    new Range(range.getEnd, range.getEnd)
  }

}

object MarkdownCodelensNotebookClient {

  implicit class JsonConvertions(json: JsonObject) {

    def toRange: Range =
      new Range(
        json.getAsJsonObject("start").toPosition,
        json.getAsJsonObject("end").toPosition
      )

    def toPosition: Position =
      new Position(
        json.getAsJsonPrimitive("line").getAsInt,
        json.getAsJsonPrimitive("character").getAsInt
      )

  }

  case class CellInText(range: Range, text: String, code: String)

}
