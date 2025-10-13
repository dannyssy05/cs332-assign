package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) => move match {
        case Left => block.left
        case Right => block.right
        case Up => block.up
        case Down => block.down
      }
    }
  }

  trait Level1 extends SolutionChecker {
      /* terrain for level 1*/

    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(!terrain(Pos(4,11)), "4,11")
    }
  }

  test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1,1))
    }
  }

  test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) == Block(goal, goal))
    }
  }

  test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }

  }
  test("TEST 1") {
    new Level1 {
      val input = Set(
        (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
        (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
      ).toStream

      val exploredSet = Set(
        Block(Pos(1,2),Pos(1,3)),
        Block(Pos(1,1),Pos(1,1))
      )

      val expected = Set(
        (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
      ).toStream

      val ret = newNeighborsOnly(input, exploredSet)
      assert(ret === expected)
    }
  }

  test("TEST 2") {
    new Level1 {
      val paths = from(Stream((startBlock, Nil)), Set(startBlock))
      assert(paths.nonEmpty)
    }
  }

  test("TEST 3") {
    new Level1 {
      val found = pathsFromStart.exists { case (b, _) => b.b1 == goal && b.b2 == goal }
      assert(found)
    }
  }

  trait Level0 extends SolutionChecker {
    val level =
      """------
        |--ST--
        |--oo--
        |--oo--
        |------""".stripMargin
    val optsolution = List(Right, Down)
  }

  test("TEST 4") {
    new Level0 {
      assert(solve(solution) == Block(goal, goal))
    }
  }


  test("TEST 6") {
    new Level1 {
      assert(solve(solution) == Block(goal, goal))
    }
  }

  test("TEST 7") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }

  test("TEST 8") {
    new SolutionChecker {
      val level =
        """S-
          |-T""".stripMargin
      assert(solution == Nil)
    }
  }

}
