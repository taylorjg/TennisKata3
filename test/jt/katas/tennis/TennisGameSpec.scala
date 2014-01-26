package jt.katas.tennis
import jt.katas.tennis.TennisGameTypes._
import org.scalatest.FunSpec

/**
 * Created by Jonathan Taylor on 25/01/2014.
 */
class TennisGameSpec extends FunSpec {

  describe("TennisGame tests") {

    describe("raw points") {

      it("has a score of (0, 0) when no points have been scored") {
        assertResult(new GameScore(0, 0))(TennisGame.gameScore(Seq.empty))
      }

      it("has a score of (1, 0) when PlayerA has scored a point") {
        val points = Seq[Point]("A")
        assertResult(new GameScore(1, 0))(TennisGame.gameScore(points))
      }

      it("has a score of (3, 1) when PlayerA has scored 3 points and PlayerB has scored 1 point") {
        val points = Seq[Point]("A", "A", "A", "B")
        assertResult(new GameScore(3, 1))(TennisGame.gameScore(points))
      }
    }

    describe("formatted game scores") {

      it("""has a score of "0 - 0" when no points have been scored""") {
        val actual = TennisGame.displayScore(Seq.empty)
        assertResult("0 - 0")(actual)
      }

      it("""has a score of "15 - 0" when PlayerA has scored a point""") {
        val points = Seq[Point]("A")
        val actual = TennisGame.displayScore(points)
        assertResult("15 - 0")(actual)
      }

      it("""has a score of "40 - 15" when PlayerA has scored 3 points and PlayerB has scored 1 point""") {
        val points = Seq[Point]("A", "A", "A", "B")
        val actual = TennisGame.displayScore(points)
        assertResult("40 - 15")(actual)
      }

      it("""has a score of "Deuce" when both players have scored 3 points""") {
        val points = Seq[Point]("A", "A", "A", "B", "B", "B")
        val actual = TennisGame.displayScore(points)
        assertResult("Deuce")(actual)
      }

      it("""has a score of "Advantage PlayerA" when PlayerA has scored 4 points and PlayerB has scored 3 points""") {
        val points = Seq[Point]("A", "A", "A", "B", "B", "B", "A")
        val actual = TennisGame.displayScore(points)
        assertResult("Advantage PlayerA")(actual)
      }

      it("""has a score of "Game PlayerA" when PlayerA has scored 5 points and PlayerB has scored 3 points""") {
        val points = Seq[Point]("A", "A", "A", "B", "B", "B", "A", "A")
        val actual = TennisGame.displayScore(points)
        assertResult("Game PlayerA")(actual)
      }
    }
  }
}
