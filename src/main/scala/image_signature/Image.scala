package image_signature

import com.letstalkdata.scalinear.{ Matrix, Vector }


object Image {
  // optimized some
  def autoCrop(m: Matrix[Int], lowerPercentile: Int, upperPercentile: Int): Matrix[Int] = {
    MatrixCalc.diff(m, axis = 0)

    // TODO investigate whether rw and cw axis are switched
    // sister repo image-match seems to be incorrect (the code is fixed in this version)
    // https://github.com/ascribe/image-match/blob/master/image_match/goldberg.py#L283
    // cumulative sum to know where to cut off
    val rw = MatrixCalc.cumsum(MatrixCalc.sum(
      // get differences between rows
      MatrixCalc.absolute(MatrixCalc.diff(m, axis = 0)),
      axis = 0))
    // same except for columns
    val cw = MatrixCalc.cumsum(MatrixCalc.sum(
      MatrixCalc.absolute(MatrixCalc.diff(m, axis = 1)),
      axis = 1))

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
  // optimized mostly
  def softenedSquareAvg(m: Matrix[Int], centerRow: Int, centerCol: Int, lowerOffset: Int,
                        upperOffset: Int): Double = {
    // enforce at least a 2x2 square
    require(upperOffset > 0, "upperOffset must be greater than 0")
    require(lowerOffset >= 0, "lowerOffset must be greater or equal to 0")

    val surroundingSquare = MatrixCalc.slice(m,
      fromRow = centerRow - lowerOffset - 1, toRow = centerRow + upperOffset + 2,
      fromCol = centerCol - lowerOffset - 1, toCol = centerCol + upperOffset + 2
    )

    val weightedSum =
      BorderedRectangle.outerCornersSum(surroundingSquare) +
        2 * BorderedRectangle.outerAdjacentToCornersSum(surroundingSquare) +
        3 * BorderedRectangle.outerEdgesSum(surroundingSquare) +
        4 * BorderedRectangle.innerCornersSum(surroundingSquare) +
        6 * BorderedRectangle.innerEdgesSum(surroundingSquare) +
        9 * BorderedRectangle.innerSum(surroundingSquare)

    val weightsSum =
      BorderedRectangle.outerCornersNum(surroundingSquare) +
        2 * BorderedRectangle.outerAdjacentToCornersNum(surroundingSquare) +
        3 * BorderedRectangle.outerEdgesNum(surroundingSquare) +
        4 * BorderedRectangle.innerCornersNum(surroundingSquare) +
        6 * BorderedRectangle.innerEdgesNum(surroundingSquare) +
        9 * BorderedRectangle.innerNum(surroundingSquare)

    weightedSum.toDouble / weightsSum
  }
}
