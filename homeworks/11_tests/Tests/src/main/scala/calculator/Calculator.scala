package tests.calculator

import scala.io.Source
import Result.Average
import Result.Min
import Result.Max
import Result.Divide

sealed trait Command
object Command {
  final case class Divide(dividend: Double, divisor: Double) extends Command
  final case class Sum(numbers: List[Double]) extends Command
  final case class Average(numbers: List[Double]) extends Command
  final case class Min(numbers: List[Double]) extends Command
  final case class Max(numbers: List[Double]) extends Command
}

final case class ErrorMessage(value: String)
object ErrorMessage {
  def fail(message: String) = Left(ErrorMessage(message))
}
import ErrorMessage._

sealed trait Result
object Result {
  final case class Divide(command: Command.Divide, value: Double) extends Result
  final case class Sum(command: Command.Sum, value: Double) extends Result
  final case class Average(command: Command.Average, value: Double)
      extends Result
  final case class Min(command: Command.Min, value: Double) extends Result
  final case class Max(command: Command.Max, value: Double) extends Result
}

object Formatter {
  def formatDouble(x: Double): String =
    if (Math.ceil(x) == x) f"$x%.0f"
    else x.toString()
}
import Formatter._

object Either {
  implicit class Extensions[L, R](v: Either[L, R]) {
    def mapError[LL](f: L => LL) =
      v match {
        case Left(l)  => Left(f(l))
        case Right(r) => Right(r)
      }
  }
}
import Either._

object Parser {
  def parse(line: String): Either[ErrorMessage, Command] = {
    val divide = CommandParser.create(Names.Divide, parseDoubles)
    val sum = CommandParser.create(Names.Sum, parseDoubles)
    val average = CommandParser.create(Names.Average, parseDoubles)
    val min = CommandParser.create(Names.Min, parseDoubles)
    val max = CommandParser.create(Names.Max, parseDoubles)
    val any = CommandParser.anyName(parseDoubles)

    line.trim().split(' ').map(_.trim).filter(_.nonEmpty).toList match {
      case divide(_, Right(args))  => createDivide(args)
      case sum(_, Right(args))     => Right { Command.Sum(args) }
      case average(_, Right(args)) => Right { Command.Average(args) }
      case min(_, Right(args))     => Right { Command.Min(args) }
      case max(_, Right(args))     => Right { Command.Max(args) }
      case any(cmd, Left(errors)) if isCommandName(cmd) =>
        fail(
          s"'$cmd' command has invalid arguments: ${errors.map(_.value.toLowerCase()).mkString(", ")}"
        )
      case cmd :: _ => fail(s"Unknown command name '$cmd'")
      case Nil      => fail("Can't process empty input")
    }
  }

  private def parseDoubles(
      args: List[String]
  ): Either[List[ErrorMessage], List[Double]] = {
    val results = args.map(x =>
      x.toDoubleOption match {
        case Some(v) => Right { v }
        case None    => fail(s"Can't parse number from '$x'")
      }
    )

    results.partition(_.isLeft) match {
      case (Nil, doubles) => Right(doubles.collect({ case Right(d) => d }))
      case (errors, _)    => Left(errors.collect({ case Left(e) => e }))
    }
  }

  private def createDivide(args: List[Double]) =
    if (args.length == 2) {
      Right { Command.Divide(args(0), args(1)) }
    } else {
      val argsError =
        args match {
          case Nil      => "none"
          case a :: Nil => s"the only ${formatDouble(a)}"
          case as =>
            s"[${args
              .map(formatDouble)
              .mkString(" ")}]"
        }

      fail(
        s"'divide' requires two arguments, but got $argsError"
      )
    }

  private object CommandParser {
    private val anyNameToken = "*"

    def create[A](name: String, parseArguments: List[String] => A) = new {
      def unapply(ss: List[String]): Option[(String, A)] =
        ss match {
          case cmd :: args
              if name == anyNameToken || cmd.equalsIgnoreCase(name) =>
            Some(cmd, parseArguments(args))
          case _ => None
        }
    }

    def anyName[A](parseArguments: List[String] => A) =
      create(anyNameToken, parseArguments)
  }

  private object Names {
    def Divide = "divide"
    def Sum = "sum"
    def Average = "average"
    def Min = "min"
    def Max = "max"
  }

  private[calculator] def allNames =
    Set(Names.Divide, Names.Sum, Names.Average, Names.Min, Names.Max)

  private def isCommandName(s: String) = allNames.contains(s)
}

object Renderer {
  def renderResult(result: Result): String = {
    result match {
      case Result.Divide(cmd, res) =>
        renderBinary("divided", cmd.dividend, cmd.divisor, res)
      case Result.Sum(cmd, res) =>
        renderList("sum", cmd.numbers, res)
      case Result.Average(cmd, res) =>
        renderList("average", cmd.numbers, res)
      case Result.Min(cmd, res) =>
        renderList("minimum", cmd.numbers, res)
      case Result.Max(cmd, res) =>
        renderList("maximum", cmd.numbers, res)
    }
  }

  private trait Formattable[A] {
    def format(a: A): String
  }

  private implicit val formattableDouble = new Formattable[Double] {
    def format(x: Double) = formatDouble(x)
  }

  private def renderBinary[A: Formattable, B: Formattable](
      name: String,
      x: A,
      y: A,
      res: B
  ) = {
    val formatA = implicitly[Formattable[A]]
    val formatB = implicitly[Formattable[B]]
    s"${formatA.format(x)} $name by ${formatA.format(y)} is ${formatB.format(res)}"
  }

  private def renderList[A: Formattable, B: Formattable](
      name: String,
      args: List[A],
      res: B
  ) = {
    val formatA = implicitly[Formattable[A]]
    val formatB = implicitly[Formattable[B]]
    s"the $name of ${args.map(formatA.format(_)).mkString(" ")} is ${formatB.format(res)}"
  }
}

object Calculator {
  def calculate(command: Command): Either[ErrorMessage, Result] =
    calculateCommand(command).mapError(
      beautifyError(command)
    )

  private def reduceToResult[A](
      args: List[A]
  )(
      op: (A, A) => A,
      createResult: A => Result
  ): Either[ErrorMessage, Result] =
    args match {
      case Nil => fail("At least one argument is required")
      case as  => Right { createResult(as.reduce(op)) }
    }

  private def calculateCommand(command: Command): Either[ErrorMessage, Result] =
    command match {
      case cmd @ Command.Divide(x, y) =>
        if (y == 0) fail(s"Can't divide ${formatDouble(x)} by zero")
        else Right { Result.Divide(cmd, x / y) }
      case cmd @ Command.Average(numbers) =>
        reduceToResult(numbers)(
          _ + _,
          sum => Result.Average(cmd, sum / numbers.length)
        )
      case cmd @ Command.Sum(numbers) =>
        reduceToResult(numbers)(_ + _, Result.Sum.curried(cmd))
      case cmd @ Command.Min(numbers) =>
        reduceToResult(numbers)(Math.min(_, _), Result.Min.curried(cmd))
      case cmd @ Command.Max(numbers) =>
        reduceToResult(numbers)(Math.max(_, _), Result.Max.curried(cmd))
    }

  private def beautifyError(cmd: Command)(e: ErrorMessage) =
    ErrorMessage(
      s"'${cmd.getClass().getSimpleName().toLowerCase()}' command failed: ${e.value.toLowerCase()}"
    )
}

import Parser._
import Renderer._
import Calculator._

object Program extends App {
  private def process(line: String): String = {
    val result = for {
      command <- parse(line)
      commandResult <- calculate(command)
    } yield renderResult(commandResult)

    result match {
      case Right(renderResult)              => renderResult
      case Left(ErrorMessage(errorMessage)) => "Error: " + errorMessage
    }
  }

  // def main(args: Array[String]): Unit =
  //   Source.stdin
  //     .getLines()
  //     .map(process)
  //     .foreach(println)

  // had no time for tests :(
  def check() = {
    val lines = List(
      "divide 4 5", // "4 divided by 5 is 0.8"
      "sum 5 5 6 8.5", // "the sum of 5 5 6 8.5 is 24.5"
      "average 4 3 8.5 4", // "the average of 4 3 8.5 4 is 4.875"
      "min 4 -3 -17", // "the minimum of 4 -3 -17 is -17"
      "max 4 -3 -17", // "the maximum of 4 -3 -17 is 4"
      "max 4", // "the maximum of 4 is 4
      "MAX 4", // "the maximum of 4 is 4
      "MaX 4", // "the maximum of 4 is 4
      "     max     4    ", // "the maximum of 4 is 4
      "     mx     4    ", // "the maximum of 4 is 4
      "unknown 4", // Error
      "divide 4 0", // Error
      "divide 42.42", // Error
      "divide 4 2 42", // Error
      "divide", // Error
      "divide fdf adf", // Error
      "sum", // Error
      "   ", // Error
      "" // Error
    )

    lines
      .map(process)
      .foreach(println)
  }

  check()
}
