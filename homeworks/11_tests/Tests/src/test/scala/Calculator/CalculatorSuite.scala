package tests.calculator
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.OptionValues
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalatest.matchers.should.Matchers
import tests.calculator.Command.Sum
import tests.calculator.Result.Average
import tests.calculator.Result.Divide
import tests.calculator.Result.Max
import tests.calculator.Result.Min
import org.scalacheck.Gen

class CalculatorSuite
    extends AnyFunSuite
    with ScalaCheckDrivenPropertyChecks
    with Matchers {
  import CalculatorSuite._

  test("zero is identity for add") {
    forAll((n: Double) => assert(add(n, 0) == n))
  }

  test("add for n ones is n") {
    forAll(positiveInteger)(n => assert(add(List.fill(n)(1.0): _*) == n))
  }

  test("add n and negative n is zero") {
    forAll(number)(n => assert(add(n, -n) == 0))
  }

  test("divide is not defined for zero divisor") {
    forAll(number)(n => assert(divide(n, 0).isEmpty))
  }

  test("divide for zero dividend is zero") {
    forAll(nonZeroNumber)(n =>
      whenever(n != 0) {
        assert(divide(0, n).value == 0)
      }
    )
  }

  test("divide result added divisor times is divident") {
    forAll(positiveInteger, positiveInteger)((n, m) =>
      whenever(m > 0) {
        val r = divide(n, m).value
        assert(add(List.fill(m)(r): _*) === n.toDouble +- 1e-10)
      }
    )
  }

  test("max is greater than or equal to every item") {
    forAll(numbers)(ns =>
      whenever(ns.nonEmpty) {
        val m = max(ns: _*)
        assert(ns.forall(m >= _))
      }
    )
  }

  test("min is less than or equal to every item") {
    forAll(numbers)(ns =>
      whenever(ns.nonEmpty) {
        val m = min(ns: _*)
        assert(ns.forall(m <= _))
      }
    )
  }

  test("average of two same numbers is number itself") {
    forAll(number)(n => assert(average(n, n) == n))
  }

  test(
    "number plus half of difference with another number is equal to these numbers average"
  ) {
    forAll(number, number)((n, m) => {
      val np = n.abs
      val mp = m.abs
      assert(min(np, mp) + ((np - mp) / 2).abs === average(np, mp) +- 1e-10)
    })
  }
}

object CalculatorSuite extends OptionValues {
  val positiveInteger = Gen.choose(1, 100)
  val number = Gen.choose(-100.0, 100.0)
  val nonZeroNumber = number.filter(_ != 0)
  val numbers = Gen.listOfN(20, number).filter(_.nonEmpty)

  def getResult(res: Result): Double =
    res match {
      case Result.Average(command, value) => value
      case Result.Divide(command, value)  => value
      case Result.Max(command, value)     => value
      case Result.Min(command, value)     => value
      case Result.Sum(command, value)     => value
    }

  def executeCommand(cmd: Command): Option[Double] =
    Calculator.calculate(cmd) match {
      case Left(e)  => None
      case Right(r) => Some { getResult(r) }
    }

  def add(numbers: Double*): Double =
    executeCommand(Command.Sum(numbers.toList)).value

  def divide(dividend: Double, divisor: Double): Option[Double] =
    executeCommand(Command.Divide(dividend, divisor))

  def average(numbers: Double*): Double =
    executeCommand(Command.Average(numbers.toList)).value

  def min(numbers: Double*): Double =
    executeCommand(Command.Min(numbers.toList)).value

  def max(numbers: Double*): Double =
    executeCommand(Command.Max(numbers.toList)).value
}
