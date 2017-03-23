package image_signature

import com.letstalkdata.scalinear.Matrix


object Image {
  def autoCrop(m: Matrix[Int], lowerPercentile: Int, upperPercentile: Int) {
    MatrixCalc.absolute(MatrixCalc.diff(m, 1))
    val rw = MatrixCalc.cumsum(MatrixCalc.sum(MatrixCalc.absolute(MatrixCalc.diff(m, 1)), 1))
    val cw = MatrixCalc.cumsum(MatrixCalc.sum(MatrixCalc.absolute(MatrixCalc.diff(m, 0)), 0))

    val rowTotal = rw.asArray.last
    val colTotal = cw.asArray.last

    val upperRowLimit = MatrixCalc.searchsorted(rw, rowTotal * upperPercentile / 100)
    val lowerRowLimit = MatrixCalc.searchsorted(rw, rowTotal * lowerPercentile / 100)

    val upperColLimit = MatrixCalc.searchsorted(cw, colTotal * upperPercentile / 100)
    val lowerColLimit = MatrixCalc.searchsorted(cw, colTotal * lowerPercentile / 100)

    MatrixCalc.slice(m, lowerRowLimit, upperRowLimit + 1, lowerColLimit, upperColLimit + 1)
  }
}
