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
  def getSoftenedSquareAvg(m: Matrix[Int], centerRow: Int, centerCol: Int, lowerOffset: Int,
                        upperOffset: Int) : Double = {

    val lowerRowBound = if (centerRow - lowerOffset > 0) centerRow - lowerOffset else 0
    val upperRowBound = if (centerRow + upperOffset < m.rows) centerRow + upperOffset else m.rows - 1
    val lowerColBound = if (centerCol - lowerOffset > 0) centerCol - lowerOffset else 0
    val upperColBound = if (centerCol + upperOffset < m.cols) centerCol + upperOffset else m.cols - 1

    val rowIndexes = lowerRowBound to upperRowBound
    val colIndexes = lowerColBound to upperColBound

    // from Goldberg paper
    val softenedSquareUpperOffset = 1
    val softenedSquareLowerOffset = 1

    val softenedSquareValues: Seq[Double] = rowIndexes.flatMap(r => colIndexes.map(c => {
      val slice =
        MatrixCalc.slice(m,
          fromRow = r - softenedSquareLowerOffset, toRow = r + softenedSquareUpperOffset + 1,
          fromCol = c - softenedSquareLowerOffset, toCol = c + softenedSquareUpperOffset + 1)

      val result: Double = MatrixCalc.avg(slice)
      result
    }))

    val sum: Double = softenedSquareValues.sum
    sum / softenedSquareValues.length
  }

  def softenedSquareAvg(m: Matrix[Int], centerRow: Int, centerCol: Int, lowerOffset: Int,
                        upperOffset: Int): Double = {
    // enforce at least a 2x2 square
    require(upperOffset > 0, "upperOffset must be greater than 0")
    require(lowerOffset >= 0, "lowerOffset must be greater or equal to 0")

    weightedAvg(
      MatrixCalc.slice(m,
        fromRow = centerRow - lowerOffset - 1, toRow = centerRow + upperOffset + 2,
        fromCol = centerCol - lowerOffset - 1, toCol = centerCol + upperOffset + 2
      )
    )
  }

  def weightedAvg(m: Matrix[Int]): Double = {
    require(m.rows >= 4 && m.cols >= 4, "must be at least a 4x4 square")

    val selectors = List(
      BorderedRectangle.outerCorners _,
      BorderedRectangle.outerAdjacentToCorners _,
      BorderedRectangle.outerEdges _,
      BorderedRectangle.innerCorners _,
      BorderedRectangle.innerEdges _,
      BorderedRectangle.inner _
    )

    val weights = List(
      1,
      2,
      3,
      4,
      6,
      9
    )

    val groups: Seq[Array[Int]] = selectors.map(selector => selector(m))

    val weightedSums = groups.zip(weights).map { case (group, weight) => group.sum * weight }

    val weightsSums = groups.zip(weights).map { case (group, weight) => group.length * weight }

    weightedSums.sum.toDouble / weightsSums.sum
  }
}
