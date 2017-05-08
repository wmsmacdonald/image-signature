package image_signature

import com.letstalkdata.scalinear.Matrix

object BorderedRectangle {

  def outerCornersNum(m: Matrix[Int]): Int = 4
  def outerCornersSum(m: Matrix[Int]): Int = {
    // top left
    m(0, 0) +
      // top right
      m(0, m.cols - 1) +
      // bottom left
      m(m.rows - 1, 0) +
      // bottom right
      m(m.rows - 1, m.cols - 1)
  }

  // pixels in the outer rim that are adjacent to the corner
  def outerAdjacentToCornersNum(m: Matrix[Int]): Int = 8
  def outerAdjacentToCornersSum(m: Matrix[Int]): Int = {
    // top left
    m(0, 1) +
      m(1, 0) +
      // top right
      m(0, m.cols - 2) +
      m(1, m.cols - 1) +
      // bottom left
      m(m.rows - 2, 0) +
      m(m.rows - 1, 1) +
      // bottom right
      m(m.rows - 2, m.cols - 1) +
      m(m.rows - 1, m.cols - 2)
  }

  def outerTopEdgeNum(m: Matrix[Int]): Int =  m.cols - 4
  def outerTopEdgeSum(m: Matrix[Int]): Int =  {
    MatrixCalc.elementSum(MatrixCalc.slice(m,
      fromRow = 0, toRow = 1,
      fromCol = 2, toCol = m.cols - 2
    ))
  }

  def outerBottomEdgeNum(m: Matrix[Int]): Int = outerTopEdgeNum(m)
  def outerBottomEdgeSum(m: Matrix[Int]): Int = {
    MatrixCalc.elementSum(MatrixCalc.slice(m,
      fromRow = m.rows - 1, toRow = m.rows,
      fromCol = 2, toCol = m.cols - 2
    ))
  }

  def outerLeftEdgeNum(m: Matrix[Int]): Int =  m.rows - 4
  def outerLeftEdgeSum(m: Matrix[Int]): Int = {
    MatrixCalc.elementSum(MatrixCalc.slice(m,
      fromRow = 2, toRow = m.rows - 2,
      fromCol = 0, toCol = 1
    ))
  }

  def outerRightEdgeNum(m: Matrix[Int]): Int = outerLeftEdgeNum(m)
  def outerRightEdgeSum(m: Matrix[Int]): Int = {
    MatrixCalc.elementSum(MatrixCalc.slice(m,
      fromRow = 2, toRow = m.rows - 2,
      fromCol = m.cols - 1, toCol = m.cols
    ))
  }

  def outerEdgesNum(m: Matrix[Int]): Int =
    outerTopEdgeNum(m) + outerLeftEdgeNum(m) +
      outerRightEdgeNum(m) + outerBottomEdgeNum(m)
  def outerEdgesSum(m: Matrix[Int]): Int = {
    outerTopEdgeSum(m) + outerLeftEdgeSum(m) +
      outerRightEdgeSum(m) + outerBottomEdgeSum(m)
  }

  def innerCornersNum(m: Matrix[Int]): Int = 4
  def innerCornersSum(m: Matrix[Int]): Int = {
    m(1, 1) +
      m(1, m.cols - 2) +
      m(m.rows - 2, 1) +
      m(m.rows - 2, m.cols - 2)
  }

  def innerTopEdgeNum(m: Matrix[Int]): Int = m.cols - 4
  def innerTopEdgeSum(m: Matrix[Int]): Int = {
    MatrixCalc.elementSum(MatrixCalc.slice(m,
      fromRow = 1, toRow = 2,
      fromCol = 2, toCol = m.cols - 2
    ))
  }

  def innerBottomEdgeNum(m: Matrix[Int]): Int = innerTopEdgeNum(m)
  def innerBottomEdgeSum(m: Matrix[Int]): Int = {
    MatrixCalc.elementSum(MatrixCalc.slice(m,
      fromRow = m.rows - 2, toRow = m.rows - 1,
      fromCol = 2, toCol = m.cols - 2
    ))
  }

  def innerLeftEdgeNum(m: Matrix[Int]): Int = m.rows - 4
  def innerLeftEdgeSum(m: Matrix[Int]): Int = {
    MatrixCalc.elementSum(MatrixCalc.slice(m,
      fromRow = 2, toRow = m.rows - 2,
      fromCol = 1, toCol = 2
    ))
  }

  def innerRightEdgeNum(m: Matrix[Int]): Int = innerLeftEdgeNum(m)
  def innerRightEdgeSum(m: Matrix[Int]): Int = {
    MatrixCalc.elementSum(MatrixCalc.slice(m,
      fromRow = 2, toRow = m.rows - 2,
      fromCol = m.cols - 2, toCol = m.cols - 1
    ))
  }

  def innerEdgesNum(m: Matrix[Int]): Int =
    innerTopEdgeNum(m) + innerBottomEdgeNum(m) +
      innerLeftEdgeNum(m) + innerRightEdgeNum(m)
  def innerEdgesSum(m: Matrix[Int]): Int =
    innerTopEdgeSum(m) + innerLeftEdgeSum(m) +
      innerRightEdgeSum(m) + innerBottomEdgeSum(m)

  def innerNum(m: Matrix[Int]): Int = (m.rows - 4) * (m.cols - 4)
  def innerSum(m: Matrix[Int]): Int = {
    MatrixCalc.elementSum(MatrixCalc.slice(m,
      fromRow = 2, toRow = m.rows - 2,
      fromCol = 2, toCol = m.cols - 2
    ))
  }
}
