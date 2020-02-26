package scala.meta.internal.metals.codeactions

import scala.meta.internal.implementation.ImplementationProvider
import org.eclipse.lsp4j.{ExecuteCommandParams, Location, Position}

import scala.meta.internal.implementation.SuperMethodProvider
import scala.meta.internal.metals.ClientCommands
import scala.meta.internal.implementation.TextDocumentWithPath
import scala.meta.internal.metals.DefinitionProvider
import scala.meta.internal.metals.JsonParser._
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.metals.codeactions.GoToSuperMethod.GoToSuperMethodParams
import scala.meta.internal.semanticdb.{SymbolInformation, SymbolOccurrence}

class GoToSuperMethod(
    definitionProvider: DefinitionProvider,
    superMethodProvider: SuperMethodProvider
) {

  def getGoToSuperMethodCommand(
      commandParams: ExecuteCommandParams
  ): Option[ExecuteCommandParams] = {
    parseJsonParams(commandParams)
      .flatMap(getGoToSuperMethodCommand)
      .map(makeCommandParams)
  }

  def getGoToSuperMethodCommand(
      params: GoToSuperMethodParams
  ): Option[Location] = {
    for {
      filePath <- params.document.toAbsolutePathSafe
      (symbolOcc, textDocument) <- definitionProvider.symbolOccurrence(
        filePath,
        params.position
      )
      symbolInformation <- ImplementationProvider.findSymbol(
        textDocument,
        symbolOcc.symbol
      )
      jumpToLocation <- {
        val docText = TextDocumentWithPath(textDocument, filePath)
        if (symbolOcc.role.isDefinition) {
          findSuperMethod(symbolInformation, symbolOcc.role, docText)
        } else {
          findDefinition(symbolInformation)
        }
      }
    } yield jumpToLocation
  }

  private def findSuperMethod(
      si: SymbolInformation,
      role: SymbolOccurrence.Role,
      docText: TextDocumentWithPath
  ): Option[Location] = {
    superMethodProvider.findSuperForMethodOrField(
      si,
      docText,
      role
    )
  }

  private def findDefinition(si: SymbolInformation): Option[Location] = {
    definitionProvider.fromSymbol(si.symbol).asScala.headOption
  }

  private def makeCommandParams(loc: Location): ExecuteCommandParams = {
    new ExecuteCommandParams(
      ClientCommands.GotoLocation.id,
      List[Object](loc).asJava
    )
  }

  private def parseJsonParams(
      commandParams: ExecuteCommandParams
  ): Option[GoToSuperMethodParams] = {
    for {
      args <- Option(commandParams.getArguments)
      argObject <- args.asScala.headOption
      pr <- argObject.toJsonObject.as[GoToSuperMethodParams].toOption
    } yield pr
  }
}

object GoToSuperMethod {

  case class GoToSuperMethodParams(document: String, position: Position)

}
