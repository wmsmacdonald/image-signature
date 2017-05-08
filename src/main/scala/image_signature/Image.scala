package image_signature

import com.letstalkdata.scalinear.{ Matrix, Vector }


object Image {
  def autoCrop(m: Matrix[Int], lowerPercentile: Int, upperPercentile: Int): Matrix[Int] = {
    MatrixCalc.absolute(MatrixCalc.diff(m, 1))

    // cumulative sum to know where to cut off
    val rw = MatrixCalc.cumsum(MatrixCalc.sum(
      // get differences between rows
      MatrixCalc.absolute(MatrixCalc.diff(m, axis = 1)),
      axis = 1))
    // same except for columns
    val cw = MatrixCalc.cumsum(MatrixCalc.sum(
      MatrixCalc.absolute(MatrixCalc.diff(m, axis = 0)),
      axis = 0))

    val rowTotal = rw.asArray.last
    val colTotal = cw.asArray.last

    // search for upper and lower row cutoffs based on given percentiles
    val upperRowLimit = MatrixCalc.searchsorted(rw, rowTotal * upperPercentile / 100)
    val lowerRowLimit = MatrixCalc.searchsorted(rw, rowTotal * lowerPercentile / 100)

    // search for upper and lower columns cutoffs based on given percentiles
    val upperColLimit = MatrixCalc.searchsorted(cw, colTotal * upperPercentile / 100)
    val lowerColLimit = MatrixCalc.searchsorted(cw, colTotal * lowerPercentile / 100)

    MatrixCalc.slice(m, lowerRowLimit, upperRowLimit + 1, lowerColLimit, upperColLimit + 1)
  }

  // get square centered on grid point at r, c
  // formed by upperOffset above/right of the target and
  // lowerOffset below/left of the target
  def getSoftenedSquare(m: Matrix[Int], centerRow: Int, centerCol: Int, lowerOffset: Int,
                        upperOffset: Int) : Matrix[Int] = {

    val lowerRowBound = if (centerRow - lowerOffset > 0) centerRow - lowerOffset else 0
    val upperRowBound = if (centerRow + upperOffset < m.rows) centerRow + upperOffset else m.rows - 1
    val lowerColBound = if (centerCol - lowerOffset > 0) centerCol - lowerOffset else 0
    val upperColBound = if (centerCol + upperOffset < m.cols) centerCol + upperOffset else m.cols - 1

    val rowIndexes = Range(lowerRowBound, upperRowBound + 1)
    val colIndexes = Range(lowerColBound, upperColBound + 1)

    // from Goldberg paper
    val softenedSquareUpperOffset = 1
    val softenedSquareLowerOffset = 1

    val softenedSquareValues = rowIndexes.map(r => colIndexes.map(c => {
      Math.round(
        MatrixCalc.avg(
          MatrixCalc.slice(m,
            fromRow = r - softenedSquareLowerOffset, toRow = r + softenedSquareUpperOffset + 1,
            fromCol = c - softenedSquareLowerOffset, toCol = r + softenedSquareUpperOffset + 1 )))
    }))

    // put into matrix
    Matrix[Int](softenedSquareValues.map(row => Vector(row:_*)):_*)
  }

  def softenedSquareAvg(m: Matrix[Int], centerRow: Int, centerCol: Int, lowerOffset: Int,
                        upperOffset: Int): Double = {
    // enforce a 2x2 square
    require(upperOffset > 0)
    require(lowerOffset >= 0)

    val surroundingSquare = MatrixCalc.slice(m,
      fromRow = centerRow - lowerOffset - 1, toRow = centerRow + upperOffset + 2,
      fromCol = centerCol - lowerOffset - 1, toCol = centerCol + upperOffset + 2
    )

    // only counted once
    val outerCorners = surroundingSquare(0, 0) +
      surroundingSquare(surroundingSquare.rows - 1, 0) +
      surroundingSquare(0, surroundingSquare.cols - 1) +
      surroundingSquare(surroundingSquare.rows - 1, surroundingSquare.cols - 1)

    // pixes in the outer rim that are adjacent to the corner
    // counted 3 times
    val outerAdjacentToCorners = 2 * (
      // top left
      surroundingSquare(0, 1) +
      surroundingSquare(1, 0) +
      // top right
      surroundingSquare(0, surroundingSquare.cols - 2) +
      surroundingSquare(1, surroundingSquare.cols - 1) +
      // bottom left
      surroundingSquare(surroundingSquare.rows - 2, 0) +
      surroundingSquare(surroundingSquare.rows - 1, 1) +
      // bottom right
      surroundingSquare(surroundingSquare.rows - 2, surroundingSquare.cols - 1) +
      surroundingSquare(surroundingSquare.rows - 1, surroundingSquare.cols - 2)
    )

    val outerTopEdge = 3 * (surroundingSquare.cols - 4) *
      MatrixCalc.elementSum(MatrixCalc.slice(surroundingSquare,
        fromRow = 0, toRow = 1,
        fromCol = 2, toCol = surroundingSquare.cols - 3
      ))

    val outerBottomEdge = 3 * (surroundingSquare.cols - 4) *
      MatrixCalc.elementSum(MatrixCalc.slice(surroundingSquare,
        fromRow = surroundingSquare.rows - 1, toRow = surroundingSquare.rows,
        fromCol = 2, toCol = surroundingSquare.cols - 3
      ))

    val outerLeftEdge = 3 * (surroundingSquare.rows - 4) *
      MatrixCalc.elementSum(MatrixCalc.slice(surroundingSquare,
        fromRow = 2, toRow = surroundingSquare.rows - 3,
        fromCol = 0, toCol = 1
      ))

    val outerRightEdge = 3 * (surroundingSquare.rows - 4) *
      MatrixCalc.elementSum(MatrixCalc.slice(surroundingSquare,
        fromRow = 2, toRow = surroundingSquare.rows - 3,
        fromCol = surroundingSquare.cols - 1, toCol = surroundingSquare.cols
      ))

    val innerCorners = 4 * (
      surroundingSquare(1, 1) +
        surroundingSquare(1, surroundingSquare.cols - 2) +
        surroundingSquare(surroundingSquare.rows - 2, 1) +
        surroundingSquare(surroundingSquare.rows - 2, surroundingSquare.cols - 2)
      )

    val innerTopEdge = 6 * (surroundingSquare.cols - 4) *
      MatrixCalc.elementSum(MatrixCalc.slice(surroundingSquare,
        fromRow = 1, toRow = 2,
        fromCol = 2, toCol = surroundingSquare.cols - 3
      ))

    val innerBottomEdge = 6 * (surroundingSquare.cols - 4) *
      MatrixCalc.elementSum(MatrixCalc.slice(surroundingSquare,
        fromRow = surroundingSquare.rows - 2, toRow = surroundingSquare.rows - 1,
        fromCol = 2, toCol = surroundingSquare.cols - 3
      ))

    val innerLeftEdge = 6 * (surroundingSquare.rows - 4) *
      MatrixCalc.elementSum(MatrixCalc.slice(surroundingSquare,
        fromRow = 2, toRow = surroundingSquare.rows - 3,
        fromCol = 1, toCol = 2
      ))

    val innerRightEdge = 6 * (surroundingSquare.rows - 4) *
      MatrixCalc.elementSum(MatrixCalc.slice(surroundingSquare,
        fromRow = 2, toRow = surroundingSquare.rows - 3,
        fromCol = surroundingSquare.cols - 2, toCol = surroundingSquare.cols - 1
      ))

    val inner = 9 * (surroundingSquare.rows - 4) * (surroundingSquare.cols - 4) *
      MatrixCalc.elementSum(MatrixCalc.slice(surroundingSquare,
        fromRow = 2, toRow = surroundingSquare.rows - 3,
        fromCol = 2, toCol = surroundingSquare.cols - 3
      ))

    val weightedSum = Array(inner, innerRightEdge, innerLeftEdge, innerBottomEdge,
      innerTopEdge, innerCorners, outerRightEdge, outerLeftEdge, outerBottomEdge,
      outerTopEdge, outerAdjacentToCorners, outerCorners).sum

    weightedSum.toDouble / surroundingSquare.size
  }
}
