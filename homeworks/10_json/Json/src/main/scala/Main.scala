import Homework._
import java.time.{LocalDate, ZonedDateTime}
import cats.syntax.traverse._

object Main extends App {
  val date = LocalDate.of(2020, 2, 14)
  val scoreboardOrError = fetchScoreboard(date)
  val scoreboard = scoreboardOrError.getOrElse(???)
  val allGameIds = scoreboard.games.map(_.gameId)
  allGameIds.foreach(id => {
    fetchGameInfo(date, id) match {
      case Left(err)   => println(s"Can't parse game with id $id, $err")
      case Right(game) => ()
    }
  })
}
