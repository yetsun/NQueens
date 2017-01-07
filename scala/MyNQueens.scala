object MyNQueens extends App{

  def queens(size: Int): Set[List[Int]] = {

    def q(row: Int): Set[List[Int]] = {
      if (row == 0) {
        Set(List())
      } else {
        for {
          oneSolution <- q(row - 1)
          col <- 0 until size
          if isSafe(col, oneSolution)
        } yield col :: oneSolution
      }
    }


    def isSafe(col: Int, solution: List[Int]): Boolean = {

      //check if it is not the same col, not diagonal
      //not the same row, implied

      val row = solution.length
      val queenRowCols = row-1 to 0 by -1 zip solution

      queenRowCols.forall(
        x => {
          x match {
            case (row1: Int, col1: Int) => col1 != col && math.abs(col1 - col) != row - row1
          }
        }
      )
    }


    q(size)

  }

  print(queens(4))


}