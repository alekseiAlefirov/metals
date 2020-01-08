package scala.meta.internal.notebooks
import scala.meta.internal.metals.Command

object NotebookCommands {

  val InsertNewCell = new Command(
    "notebook-insert-new-cell",
    "Insert new cell",
    "Insert new cell"
  )

  val RunCell = new Command(
    "notebook-run-cell",
    "Run cell",
    "Run cell"
  )

  def all: List[Command] = List(InsertNewCell, RunCell)

  implicit class NotebookCommandConversions(cmd: String) {
    def isNotebookCommand: Boolean = cmd.startsWith("notebook-")
  }

}
