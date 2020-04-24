package tests.pc

import org.eclipse.lsp4j.Location
import scala.meta.internal.metals.CompilerOffsetParams
import scala.meta.internal.jdk.CollectionConverters._
import tests.BuildInfoVersions

class TypeDefinitionSuite extends BasePcDefinitionSuite {

  override def excludedScalaVersions: Set[String] =
    Set(BuildInfoVersions.scala3)

  override def requiresJdkSources: Boolean = true

  override def requiresScalaLibrarySources: Boolean = true

  override def definitions(offsetParams: CompilerOffsetParams): List[Location] =
    presentationCompiler.typeDefinition(offsetParams).get().asScala.toList

  check(
    "val",
    """
      |<<class TClass(i: Int)>>
      |
      |object Main {
      |  val ts@@t = new TClass(2)
      |}""".stripMargin
  )

  check(
    "constructor",
    """
      |<<class TClass(i: Int) {}>>
      |
      |object Main {
      | def tst(m: TClass): Unit = {}
      |
      |  tst(new T@@Class(2))
      |}""".stripMargin
  )

  check(
    "method",
    """
      |object Main {
      |  def tst(): Unit = {}
      |
      |  ts@@/*scala/Unit# Unit.scala*/t()
      |}""".stripMargin
  )

  check(
    "named-parameter",
    """
      |object Main {
      |  def tst(par1: Int, par2: String): Unit = {}
      |
      |  tst(p/*java/lang/String# String.java*/@@ar2 = "foo", par1 = 1)
      |}""".stripMargin
  )

  check(
    "list",
    """
      |object Main {
      | List(1).hea/*scala/Int# Int.scala*/@@d
      |}
      |""".stripMargin
  )

  check(
    "class",
    """
      |object Main {
      | <<class F@@oo(val x: Int)>>
      |}
      |""".stripMargin
  )

  check(
    "val-keyword",
    """
      |object Main {
      | va@@l x = 42
      |}
      |""".stripMargin
  )

  check(
    "literal",
    """
      |object Main {
      | val x = 4/*scala/Int# Int.scala*/@@2
      |}
      |""".stripMargin
  )

  check(
    "if",
    """
      |object Main {
      | for {
      |   x <- List(1)
      |   i/*scala/collection/generic/FilterMonadic# FilterMonadic.scala*/@@f x > 1
      | } println(x)
      |}
      |""".stripMargin
  )

  check(
    "string",
    """
      |object Main {
      | "".stripS/*java/lang/String# String.java*/@@uffix("foo")
      |}
      |""".stripMargin
  )

  check(
    "method-generic",
    """
      |object Main {
      | def foo[<<T>>](param: T): T = para@@m
      |}
      |""".stripMargin
  )

  check(
    "method-generic-result",
    """
      |object A {
      | def foo[T](param: T): T = param
      }
      |object Main {
      | println(A.fo/*scala/Int# Int.scala*/@@o(2))
      |}
      |""".stripMargin
  )

}
