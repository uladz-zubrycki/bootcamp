package tests.calculator
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.EitherValues
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalatest.matchers.should.Matchers
import org.scalacheck.Gen

class ParserSuite extends AnyFunSuite {
  import ParserSuite._

  test("should fail on empty input") {
    assertFails(emptyInput)
  }

  test("should fail on invalid command without arguments") {
    assertFails(commandInput(invalidCommandName, emptyInput))
  }

  test("should fail on invalid command with valid arguments") {
    assertFails(commandInput(invalidCommandName, validArguments))
  }

  test("should fail on invalid command with invalid arguments") {
    assertFails(commandInput(invalidCommandName, invalidArguments))
  }

  test("should fail on valid command without arguments") {
    assertFails(commandInput(validCommandName, emptyInput))
  }

  test("should fail on valid command with invalid arguments") {
    assertFails(commandInput(validCommandName, invalidArguments))
  }

  test("should parse valid command with valid arguments") {
    assertParses(commandInput(validCommandName, validArguments))
  }
}

object ParserSuite extends ScalaCheckDrivenPropertyChecks with Matchers {
  def assertFails(input: Gen[String]) =
    forAll(input)(s => Parser.parse(s).isLeft should be)

  def assertParses(input: Gen[String]) =
    forAll(input)(s => Parser.parse(s).isRight should be)

  val emptyInput = for {
    n <- Gen.choose(0, 100)
  } yield List.fill(n)(" ").mkString

  val validCommandName = Gen.oneOf(Parser.allNames)
  val invalidCommandName = Gen.alphaStr.filterNot(Parser.allNames.contains)

  val validArguments = Gen
    .listOfN(20, Gen.choose(-1000.0, 1000.0))
    .filter(_.length > 0)
    .map(_.mkString(" "))

  val invalidArguments = Gen
    .listOfN(20, Gen.alphaNumStr.filterNot(s => s.toIntOption.isDefined))
    .filter(_.length > 0)
    .map(_.mkString(" "))

  def commandInput(commandName: Gen[String], args: Gen[String]): Gen[String] =
    for {
      name <- commandName
      args <- args
    } yield s"$name $args"
}
