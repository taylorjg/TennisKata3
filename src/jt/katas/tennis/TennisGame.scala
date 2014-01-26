package jt.katas.tennis
import TennisGameTypes._

/**
 * Created by Jonathan Taylor on 25/01/2014.
 */
object TennisGameTypes {

  trait Point
  case class PlayerA() extends Point
  case class PlayerB() extends Point

  implicit def stringToPoint(s: String) =
    s match {
      case "A" => PlayerA()
      case "B" => PlayerB()
    }

  type GameScore = (Int, Int)
}

object TennisGame {

  def gameScore(points: Seq[Point]) =
    points.foldLeft(new GameScore(0, 0))(updateGame)

  def displayScore(points: Seq[Point]) =
    displayGame(gameScore(points))

  private def updateGame(gameScore: GameScore, point: Point) =
    point match {
      case PlayerA() => gameScore.copy(_1 = gameScore._1 + 1)
      case PlayerB() => gameScore.copy(_2 = gameScore._2 + 1)
    }

  private def displayGame(gameScore: GameScore) = {

    def displayScore(score: Int) =
      score match {
        case 0 => "0"
        case 1 => "15"
        case 2 => "30"
        case 3 => "40"
        case _ => throw new Exception(s"Invalid score: $score")
      }

    gameScore match {
      case (a, b) if a + b >= 6 && a == b => "Deuce"
      case (a, b) if a + b >= 6 && a == b + 1 => "Advantage PlayerA"
      case (a, b) if a + b >= 6 && b == a + 1 => "Advantage PlayerB"
      case (a, b) if a >= 4 && a - b >= 2 => "Game PlayerA"
      case (a, b) if b >= 4 && b - a >= 2 => "Game PlayerB"
      case (a, b) => "%s - %s".format(displayScore(a), displayScore(b))
    }
  }
}
