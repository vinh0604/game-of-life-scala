import scala.util.Random;

/**
 * Created by vinhbachsy on 13/10/15.
 */
object GameOfLife {
  def main(args: Array[String]) = {
    var board = generateBoard(10,10)
    for (i <- 1 to 100) {
      println(s"Gen ${i}")
      println(printBoard(board))
      board = nextGen(board)
    }
  }

  def nextGen(board: List[List[Cell]]): List[List[Cell]] = {
    board.map(_.map(_.evolve(board)))
  }

  def generateBoard(width: Int, height: Int): List[List[Cell]] = {
    (0 to height - 1).toList.map {
      y => (0 to width - 1).toList.map {
        x => if(Random.nextBoolean()) LiveCell(x, y) else DeadCell(x,y)
      }
    }
  }

  def printBoard(board: List[List[Cell]]): String = {
    board.map {
      row => row.map {
        cell => if(cell.isInstanceOf[LiveCell]) "*" else "_"
      }.mkString(" ")
    }.mkString("\n")
  }
}

abstract class Cell(x: Int, y: Int) {
  def neighbors(board: List[List[Cell]]): List[Cell] = {
    val neighborsPos = List((x-1,y-1), (x, y-1), (x+1, y-1),
                            (x-1, y), (x+1, y),
                            (x-1, y+1), (x, y+1), (x+1, y+1))
    val width = board(0).length
    val height = board.length

    neighborsPos.filter {
      case (xPos, yPos) => xPos >= 0 && yPos >= 0 && xPos < width && yPos < height
    }.map {
      case (xPos, yPos) => board(yPos)(xPos)
    }
  }
  def evolve(board: List[List[Cell]]): Cell
}
case class DeadCell(x: Int, y: Int) extends Cell(x, y) {
  override def evolve(board: List[List[Cell]]): Cell = {
    val liveCellCount = neighbors(board).count(_.isInstanceOf[LiveCell])
    if (liveCellCount == 3) return LiveCell(x,y)
    this
  }
}
case class LiveCell(x: Int, y: Int) extends Cell(x, y) {
  override def evolve(board: List[List[Cell]]): Cell = {
    val liveCellCount = neighbors(board).count(_.isInstanceOf[LiveCell])
    if (liveCellCount < 2 || liveCellCount > 3) return DeadCell(x,y)
    this
  }
}
