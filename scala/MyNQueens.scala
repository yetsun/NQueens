/*

The idea is to place the queens row by row. (start from row 0. each row has one queen)
When placing a queen on row k, we assume that the solutions for row from row 0 to row k-1 are ready. 
(Invariant k-1 solution exists)
Then the only thing left is to check each column on row k is OK for the existing solutions of row 0 to row k-1.

*/


object MyNQueens extends App{


  /*
   * Return value is a set of list. 
   * Each list is a solution. 
   * The list index is the row number is reverse order. The last row is the first element in the list 
   * (This is due to the the nature scala list, i.e. it's easier to append to the head)
   * The list value is the column index.
   */
  def queens(boardSize: Int): Set[List[Int]] = {

    def q(row: Int): Set[List[Int]] = {
      if (row == 0) {
        Set(List())
      } else {
        for {
          oneSolution <- q(row - 1)
          newQueenCol <- 0 until boardSize
          if isSafe(newQueenCol, oneSolution)
        } yield newQueenCol :: oneSolution
      }
    }


    /*
     * check if it is not the same col, not diagonal
     * not the same row, implied
     */
    def isSafe(newQueenCol: Int, solution: List[Int]): Boolean = {
      val newQueenRow = solution.length
      val queenRowCols = newQueenRow - 1 to 0 by -1 zip solution

      queenRowCols forall {
        case (oldQueenRow: Int, oldQueenCol: Int) => oldQueenCol != newQueenCol && math.abs(oldQueenCol - newQueenCol) != newQueenRow - oldQueenRow
      }

    }


    q(boardSize)

  }

  print(queens(4))


}