package jt.katas.tennis
import jt.katas.tennis.TennisGameTypes._
import org.scalatest.FunSpec
import org.scalatest.prop.TableDrivenPropertyChecks._

/**
 * Created by Jonathan Taylor on 25/01/2014.
 */
class TennisGameSpec extends FunSpec {

  private def generatePoints(numPoints1: Int, numPoints2: Int): Seq[Point] = {
    val total = numPoints1 + numPoints2
    val alternationLimit = 2 * math.min(numPoints1, numPoints2)
    Seq.tabulate(total)(n => {
      if (n < alternationLimit) {
        if (n % 2 == 0) "A" else "B"
      }
      else {
        if (numPoints1 > numPoints2) "A" else "B"
      }
    })
  }

  describe("TennisGame tests") {

    describe("raw points score") {

      val rawPointsScoreTable = Table(
        ("numPoints1", "numPoints2"),
        (0,            0),
        (1,            0),
        (2,            0),
        (3,            0),
        (1,            1),
        (1,            2),
        (1,            3),
        (3,            3),
        (4,            3),
        (3,            4),
        (4,            4),
        (5,            5),
        (6,            4),
        (4,            6)
      )

      it("has the correct raw points score when various numbers of points have been won") {
        forAll (rawPointsScoreTable) { (numPoints1, numPoints2) =>
          val expected = new GameScore(numPoints1, numPoints2)
          val points = generatePoints(numPoints1, numPoints2)
          val actual = TennisGame.gameScore(points)
          assertResult(expected)(actual)
        }
      }
    }

    describe("display score") {

      val displayScoreTable = Table(
        ("numPoints1", "numPoints2", "expected"),
        (0,            0,            "0 - 0"),
        (1,            0,            "15 - 0"),
        (2,            0,            "30 - 0"),
        (3,            0,            "40 - 0"),
        (1,            1,            "15 - 15"),
        (1,            2,            "15 - 30"),
        (1,            3,            "15 - 40"),
        (2,            2,            "30 - 30"),
        (3,            3,            "Deuce"),
        (4,            3,            "Advantage PlayerA"),
        (3,            4,            "Advantage PlayerB"),
        (4,            4,            "Deuce"),
        (5,            4,            "Advantage PlayerA"),
        (4,            5,            "Advantage PlayerB"),
        (5,            5,            "Deuce"),
        (4,            0,            "Game PlayerA"),
        (0,            4,            "Game PlayerB"),
        (5,            3,            "Game PlayerA"),
        (3,            5,            "Game PlayerB")
      )

      it("has the correct display score when various numbers of points have been won") {
        forAll (displayScoreTable) { (numPoints1, numPoints2, expected) =>
          val points = generatePoints(numPoints1, numPoints2)
          val actual = TennisGame.displayScore(points)
          assertResult(expected)(actual)
        }
      }
    }
  }
}
