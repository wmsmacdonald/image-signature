package image_signature

import com.letstalkdata.scalinear.{ Matrix, Vector }

object SignatureCalc {

  def getGridCoords(rows: Int, cols: Int, numBlocksHigh: Int, numBlocksWide: Int): List[(Int, Int)] = {
    val squareHeight: Double = rows.toDouble / numBlocksHigh
    val squareWidth: Double = cols.toDouble / numBlocksHigh

    // TODO check for floating point error
    val rowsIndexes: List[Int] = (squareHeight until numBlocksHigh by squareHeight).toList.map(_.toInt)
    val colsIndexes: List[Int] = (squareWidth until numBlocksWide by squareWidth).toList.map(_.toInt)
    assert(rowsIndexes.length == numBlocksHigh - 1)
    assert(colsIndexes.length == numBlocksWide - 1)

    // return tuples of index pairs (row, column)
    for { r <- rowsIndexes; c <- colsIndexes } yield (r, c)
  }

  def getSquares(m: Matrix[Int], coords: List[(Int, Int)],
                 numBlocksHigh: Int, numBlocksWide: Int): List[Matrix[Int]] = {

    // from Goldberg paper
    // first we get a P x P square centered at grid point
    val P: Int = math.max(2, math.floor(0.5 + math.min(m.rows, m.cols) / 20).toInt)

    // if P = 4, upperOffset = 2 and lowerOffset = 1
    val upperOffset: Int = math.ceil((P - 1.0) / 2).toInt
    val lowerOffset: Int = math.floor((P - 1.0) / 2).toInt

    coords.map { case (r, c) =>
      MatrixCalc.slice(m,
        fromRow = r - lowerOffset,
        toRow = r + upperOffset + 1,
        fromCol = c - lowerOffset,
        toCol = c + upperOffset + 1)
    }
  }
}
