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

  def softenedSquareAvg(m: Matrix[Int], centerRow: Int, centerCol: Int, lowerOffset: Int,
                        upperOffset: Int): Double = {

    require(upperOffset > 0 && lowerOffset >= 0, "must be at least a 2x2 square")

    weightedAvg(
      MatrixCalc.slice(m,
        fromRow = centerRow - lowerOffset - 1, toRow = centerRow + upperOffset + 2,
        fromCol = centerCol - lowerOffset - 1, toCol = centerCol + upperOffset + 2
      )
    )
  }

  def weightedAvg(m: Matrix[Int]): Double = {
    require(m.rows >= 4 && m.cols >= 4, "must be at least a 4x4 square")

    // selectors: functions that operate on a Matrix and return the elements in the group
    // weights: how many times the pixel is counted in the average
    val (selectors, weights) = List(
      (BorderedRectangle.outerCorners _, 1),
      (BorderedRectangle.outerAdjacentToCorners _, 2),
      (BorderedRectangle.outerEdges _, 3),
      (BorderedRectangle.innerCorners _, 4),
      (BorderedRectangle.innerEdges _, 6),
      (BorderedRectangle.inner _, 9)
    ).unzip

    val groups: Seq[Array[Int]] = selectors.map(selector => selector(m))

    val weightedSums = groups.zip(weights).map { case (group, weight) => group.sum * weight }

    val weightsSums = groups.zip(weights).map { case (group, weight) => group.length * weight }

    weightedSums.sum.toDouble / weightsSums.sum
  }
}
