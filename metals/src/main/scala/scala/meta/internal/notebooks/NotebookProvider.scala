package scala.meta.internal.notebooks

import scala.meta.io.AbsolutePath
import org.eclipse.lsp4j.CodeLens
import scala.concurrent.Future
import almond.ScalaInterpreter
import scala.collection.concurrent.TrieMap
import almond.ScalaInterpreterParams
import scala.meta.internal.metals.BuildTargets
import scala.meta.internal.metals.MetalsEnrichments._
import java.io.File
import java.net.URLClassLoader
import coursierapi._
import scala.meta.internal.metals.Embedded

import almond.interpreter.ExecuteResult
import java.net.URL
import almond.interpreter.api.OutputHandler
import almond.interpreter.api.DisplayData

class NotebookProvider(
    buildTargets: BuildTargets,
    notebookClient: NotebookClient
) {

  //TODO: is path good identifier?
  //what about renames?
  //renames actually can be problem for almond as well - in case of persisting history
  private val interpreterSessions: TrieMap[AbsolutePath, ScalaInterpreter] =
    TrieMap.empty

  def findCodeLenses(path: AbsolutePath): Seq[CodeLens] = {
    notebookClient.codeLenses(path)
  }

  def executeCommand(id: String, args: Seq[Object]): Future[_] = {
    notebookClient.executeCommand(id, args, this)
  }

  //todo: add execution count
  def execute(notebookPath: AbsolutePath, code: String): ExecuteResult = {
    scribe.info("NOTEBOOK: {" + code + "}")
    interpreterSessions
      .getOrElse(notebookPath, initialize(notebookPath))
      .execute(
        "val a = 1" /*code*/,
        storeHistory = false,
        inputManager = None,
        outputHandler = Some(new MyOutputHandler())
      )
  }

  //todo: shutdown
  private def initialize(notebookPath: AbsolutePath): ScalaInterpreter = {
    val cl = newAlmondClassLoader()
    //NOTE (alekseiAlefirov): we need to set current thread ClassLoaders, because otherwise
    //Ammonite can not load almond's created resources
    val originalCl = Thread.currentThread().getContextClassLoader()
    try {
      Thread.currentThread().setContextClassLoader(cl)
      val interpreter = new ScalaInterpreter(
        new ScalaInterpreterParams(
          initialClassLoader = almondExecutionClassLoader(notebookPath, cl) /*,
          autoUpdateLazyVals = false,
          autoUpdateVars = false*/
        )
      )
      interpreter.init()
      interpreterSessions.update(notebookPath, interpreter)
      interpreter
    } finally {
      Thread.currentThread().setContextClassLoader(originalCl)
    }
  }

  //TODO: work out the versions stuff
  def newAlmondClassLoader(): ClassLoader = {
    val almond = Dependency.of(
      "sh.almond",
      s"scala-interpreter_2.12.10",
      "0.9.0"
    )
    //TODO: can we solve jitpack/jvm-repr problem with embedding it here somehow?
    val jars = Embedded.fetchSettings(almond, "2.12.10").fetch()
    val parent =
      new AlmondClassLoader(this.getClass.getClassLoader)
    val urls = jars.iterator.asScala.map(_.toURI().toURL()).toArray
    scribe.info(s"NOTEBOOK, almond deps jars: ${urls.mkString("\n")}")
    new URLClassLoader(urls, parent)
  }

  class AlmondClassLoader(parent: ClassLoader) extends ClassLoader(null) {
    override def findClass(name: String): Class[_] = {
      val isShared = name.startsWith("almond")
      if (isShared) {
        parent.loadClass(name)
      } else {
        throw new ClassNotFoundException(name)
      }
    }
    override def findResource(name: String): URL = {
      val isShared = name.startsWith("almond")
      if (isShared) {
        parent.getResource(name)
      } else {
        null
      }
    }
  }

  def almondExecutionClassLoader(
      notebookPath: AbsolutePath,
      almondCl: ClassLoader
  ): ClassLoader = {
    val bt = buildTargets.inverseSources(notebookPath)
    val classpath = buildTargets
      .scalaTarget(bt.get)
      .get
      .fullClasspath
      .asScala
      .map(path => new File(path.toString()).toURI.toURL)
      .toArray
    scribe.info(s"NOTEBOOK, target classpath: ${classpath.mkString("\n")}")

    new URLClassLoader(
      classpath,
      almondCl
    )
  }

  class MyOutputHandler() extends OutputHandler {
    override def display(displayData: DisplayData): Unit =
      scribe.info("NOTEBOOK ALMOND " + displayData.toString())
    override def stderr(s: String): Unit = scribe.error("NOTEBOOK ALMOND " + s)
    override def stdout(s: String): Unit = scribe.info("NOTEBOOK ALMOND " + s)
    override def updateDisplay(displayData: DisplayData): Unit = ???
  }

}
