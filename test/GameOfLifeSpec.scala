import org.scalatest._

/**
 * Created by vinhbachsy on 18/10/15.
 */
class GameOfLifeSpec extends FlatSpec with Matchers {
  it should "be false" in {
    false should be (false)
  }

  "neighbors" should "return list of neighbor of a cell" in {
    val board = List(
      List(DeadCell(0,0), LiveCell(1,0), LiveCell(2,0)),
      List(DeadCell(0,1), LiveCell(1,1), LiveCell(2,1)),
      List(DeadCell(0,2), LiveCell(1,2), LiveCell(2,2))
    )

    LiveCell(1,1).neighbors(board) should be (
      List(DeadCell(0,0), LiveCell(1,0), LiveCell(2,0),
            DeadCell(0,1), LiveCell(2,1),
            DeadCell(0,2), LiveCell(1,2), LiveCell(2,2)))

    DeadCell(0,0).neighbors(board) should be (
      List(LiveCell(1,0), DeadCell(0,1), LiveCell(1,1))
    )
  }

  "A DeadCell" should "evolve to LiveCell if it has 3 LiveCell neighbors" in {
    val board = List(
      List(DeadCell(0,0), LiveCell(1,0), LiveCell(2,0)),
      List(DeadCell(0,1), LiveCell(1,1), LiveCell(2,1)),
      List(DeadCell(0,2), LiveCell(1,2), LiveCell(2,2))
    )

    DeadCell(0,1).evolve(board) should be (LiveCell(0,1))
  }

  "A DeadCell" should "not evolve to LiveCell if it doesn't have 3 LiveCell neighbors" in {
    val board = List(
      List(DeadCell(0,0), LiveCell(1,0), LiveCell(2,0)),
      List(DeadCell(0,1), LiveCell(1,1), LiveCell(2,1)),
      List(DeadCell(0,2), LiveCell(1,2), LiveCell(2,2))
    )

    DeadCell(0,0).evolve(board) should be (DeadCell(0,0))
    DeadCell(0,2).evolve(board) should be (DeadCell(0,2))
  }

  "A LiveCell" should "evolve to DeadCell if it has less than 2 LiveCell neighbors" in {
    val board = List(
      List(DeadCell(0,0), LiveCell(1,0)),
      List(DeadCell(0,1), DeadCell(1,1))
    )

    LiveCell(1,0).evolve(board) should be (DeadCell(1,0))
  }

  "A LiveCell" should "evolve to DeadCell if it has more than 3 LiveCell neighbors" in {
    val board = List(
      List(DeadCell(0,0), LiveCell(1,0), LiveCell(2,0)),
      List(DeadCell(0,1), LiveCell(1,1), LiveCell(2,1)),
      List(DeadCell(0,2), LiveCell(1,2), LiveCell(2,2))
    )

    LiveCell(1,1).evolve(board) should be (DeadCell(1,1))
    LiveCell(2,1).evolve(board) should be (DeadCell(2,1))
  }

  "nextGen" should "return new board with evolved cell of each cell in current board" in {
    val board = List(
      List(DeadCell(0,0), LiveCell(1,0), LiveCell(2,0)),
      List(DeadCell(0,1), LiveCell(1,1), LiveCell(2,1)),
      List(DeadCell(0,2), LiveCell(1,2), LiveCell(2,2))
    )

    GameOfLife.nextGen(board) should be (List(
      List(DeadCell(0,0), LiveCell(1,0), LiveCell(2,0)),
      List(LiveCell(0,1), DeadCell(1,1), DeadCell(2,1)),
      List(DeadCell(0,2), LiveCell(1,2), LiveCell(2,2))
    ))
  }

  "generateBoard" should "return new board with specified height and width" in {
    val width = 4
    val height = 3
    val board = GameOfLife.generateBoard(width, height)
    board.length should be (3)
    board(0).length should be (4)
  }

  "generateBoard" should "return generate DeadCell and LiveCell" in {
    val width = 4
    val height = 3
    val board = GameOfLife.generateBoard(width, height)
    val liveCellCount = board.flatten.count(_.isInstanceOf[LiveCell])
    val deadCellCount = board.flatten.count(_.isInstanceOf[DeadCell])
    (liveCellCount + deadCellCount) should be (12)
  }

  "printBoard" should "output board with * as LiveCell and _ as DeadCell" in {
    val board = List(
      List(DeadCell(0,0), LiveCell(1,0), LiveCell(2,0)),
      List(DeadCell(0,1), LiveCell(1,1), LiveCell(2,1)),
      List(DeadCell(0,2), LiveCell(1,2), LiveCell(2,2))
    )

    GameOfLife.printBoard(board) should be ("_ * *\n_ * *\n_ * *")
  }
}
