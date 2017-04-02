package image_signature

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

  def normalizeValue(equalCutoff: Int, lighterCutoff: Int,
                     darkerCutoff: Int)(value: Int): Int = {
    if (value < -equalCutoff) // darker
      if (value < darkerCutoff) -2 else -1 // much darker or just darker
    else if (value > equalCutoff) // lighter
      if (value > lighterCutoff) 2 else 1 // much lighter or just lighter
    else 0 // equal
  }

  def computeNormalizer(differenceGroups: List[Array[Int]], equalCutoff: Int = 2): (Int => Int) = {
    // all differences that are lighter
    val lighter = differenceGroups.flatten.filter(_ > 2)
    // all differences that are darker
    val darker = differenceGroups.flatten.filter(_ < -2)

    // get median as cutoff between lighter and much lighter
    val lighterCutoff: Int = lighter.sorted.apply(lighter.length / 2)
    // get median as cutoff between darker and much darker
    val darkerCutoff: Int = darker.sorted.apply(darker.length / 2)

    // function normalizes values to much darker, darker, equal, lighter, much lighter
    // represented by -2, -1, 0, 1, 2 respectively
    SignatureCalc.normalizeValue(2, lighterCutoff, darkerCutoff)(_)
  }

}
