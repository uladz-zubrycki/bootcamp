package adt
import scala.io.StdIn

sealed trait Suit
case object Diamonds extends Suit
case object Clubs extends Suit
case object Hearts extends Suit
case object Spades extends Suit

sealed trait Rank
case object Ace extends Rank
case object King extends Rank
case object Queen extends Rank
case object Jack extends Rank
case object Ten extends Rank
case object Nine extends Rank
case object Eight extends Rank
case object Seven extends Rank
case object Six extends Rank
case object Five extends Rank
case object Four extends Rank
case object Three extends Rank
case object Two extends Rank

final case class Card(suit: Suit, rank: Rank)

abstract class Hand(val cards: List[Card])
abstract case class TexasHand private (override val cards: List[Card])
    extends Hand(cards)
abstract case class OmahaHand private (override val cards: List[Card])
    extends Hand(cards)

object LimitedCardSet {
  def create[A](
      cards: List[Card],
      limit: Int,
      create: List[Card] => A
  ): Either[String, A] =
    if (cards.length == limit) Right { create(cards) }
    else Left { s"$limit cards expected, but got ${cards.length}" }
}

object TexasHand {
  def create(cards: List[Card]) =
    LimitedCardSet.create[TexasHand](cards, 3, cs => new TexasHand(cs) {})
}

object OmahaHand {
  def create(cards: List[Card]) =
    LimitedCardSet.create[OmahaHand](cards, 4, cs => new OmahaHand(cs) {})
}

abstract case class Board private (cards: List[Card])
object Board {
  def create(cards: List[Card]) =
    LimitedCardSet.create[Board](cards, 5, cs => new Board(cs) {})
}

abstract case class Combination private (cards: List[Card])
object Combination {
  def create(cards: List[Card]) =
    LimitedCardSet.create[Combination](cards, 5, cs => new Combination(cs) {})
}

sealed trait CombinationValue
case class HighCard(rank: Rank) extends CombinationValue
case class Pair(fst: Card, snd: Card) extends CombinationValue
case class TwoPairs(fst: Pair, snd: Pair) extends CombinationValue
case class ThreeOfKind(rank: Rank) extends CombinationValue
case class Straight(cards: Combination) extends CombinationValue
case class Flush(suit: Suit) extends CombinationValue
case class FullHouse(fst: ThreeOfKind, snd: Pair) extends CombinationValue
case class FourOfKind(rank: Rank) extends CombinationValue
case class StraightFlush(fst: Straight, snd: Flush) extends CombinationValue

final case class EqualHands(hands: List[Hand])
final case class GameResult(hands: List[EqualHands])

abstract class Game(val board: Board, val hands: List[Hand])
final case class TexasGame(
    override val board: Board,
    override val hands: List[TexasHand]
) extends Game(board, hands)
object TexasGame {
  def combine(board: Board, hand: Hand): List[Combination] = ???
}

final case class OmahaGame(
    override val board: Board,
    override val hands: List[OmahaHand]
) extends Game(board, hands)
object OmahaGame {
  def combine(board: Board, hand: Hand): List[Combination] = ???
}

object Evaluator {
  def evaluateCombination(combination: Combination): CombinationValue = ???

  implicit val CombinationValueOrdering: Ordering[CombinationValue] = (x, y) =>
    ???

  def orderHands(hands: List[(Hand, CombinationValue)]): List[EqualHands] = ???

  def evaluate(
      game: Game,
      combine: (Board, Hand) => List[Combination]
  ): List[EqualHands] = {
    val evaluatedHands =
      game.hands
        .map(h => {
          val combinations = combine(game.board, h)
          val highestValue =
            combinations
              .map(evaluateCombination)
              .max
          (h, highestValue)
        })
    orderHands(evaluatedHands)
  }
}

object Main extends App {
  def parse(input: String): Either[String, Game] = ???

  def play(game: Game): GameResult = {
    val hands = game match {
      case g: TexasGame => Evaluator.evaluate(g, TexasGame.combine)
      case g: OmahaGame => Evaluator.evaluate(g, OmahaGame.combine)
    }

    GameResult { hands }
  }

  def format(result: GameResult): String = ???

  def process(input: String): String = {
    parse(input) match {
      case Right(game) => (play _ andThen format _)(game)
      case Left(error) => s"Error: $error"
    }
  }

  Iterator
    .continually(Option(StdIn.readLine()))
    .takeWhile(_.nonEmpty)
    .foreach { x =>
      x.map(process).foreach(println)
    }
}
