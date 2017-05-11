package image_signature

import com.letstalkdata.scalinear.Matrix

// Matrix must be at least 4x4
// Rectangle has 11 partitions/groups
// Used to calculate grid point averages with different weights for each partition/group
// [outerCorners            outerAdjacentToCorners  outerTopEdge  ...  outerTopEdge  outerAdjacentToCorners  outerCorners          ]
// [outerAdjacentToCorners  innerCorner             innerTopEdge  ...  innerTopEdge  innerCorner             outerAdjacentToCorners]
// [outerLeftEdge           innerLeftEdge           inner         ...  inner         innerRightEdge          outerRightEdge        ]
// [...                     ...                     ...           ...  ...           ...                     ...                   ]
// [outerLeftEdge           innerLeftEdge           inner         ...  inner         innerRightEdge          outerRightEdge        ]
// [outerAdjacentToCorners  innerCorner             innerTopEdge  ...  innerTopEdge  innerCorner             outerAdjacentToCorners]
// [outerCorners            outerAdjacentToCorners  outerTopEdge  ...  outerTopEdge  outerAdjacentToCorners  outerCorners          ]


object BorderedRectangle {

  def outerCorners(m: Matrix[Int]): Array[Int] = {
    require(m.rows >= 4 && m.cols >= 4, "matrix must be at least 4x4")

    Array(
      // top left
      m(0, 0),
      // top right
      m(0, m.cols - 1),
      // bottom left
      m(m.rows - 1, 0),
      // bottom right
      m(m.rows - 1, m.cols - 1)
    )
  }

  // pixels in the outer rim that are adjacent to the corner
  def outerAdjacentToCorners(m: Matrix[Int]): Array[Int] = {
    require(m.rows >= 4 && m.cols >= 4, "matrix must be at least 4x4")

    // ordered left to right, top to bottom
    Array(
      m(0, 1),
      m(0, m.cols - 2),
      m(1, 0),
      m(1, m.cols - 1),
      m(m.rows - 2, 0),
      m(m.rows - 2, m.cols - 1),
      m(m.rows - 1, 1),
      m(m.rows - 1, m.cols - 2)
    )
  }

  def outerTopEdge(m: Matrix[Int]): Array[Int] =  {
    require(m.rows >= 4 && m.cols >= 4, "matrix must be at least 4x4")

    MatrixCalc.slice(m,
      fromRow = 0, toRow = 1,
      fromCol = 2, toCol = m.cols - 2
    ).flatten.asArray
  }

  def outerBottomEdge(m: Matrix[Int]): Array[Int] = {
    require(m.rows >= 4 && m.cols >= 4, "matrix must be at least 4x4")

    MatrixCalc.slice(m,
      fromRow = m.rows - 1, toRow = m.rows,
      fromCol = 2, toCol = m.cols - 2
    ).flatten.asArray
  }

  def outerLeftEdge(m: Matrix[Int]): Array[Int] = {
    require(m.rows >= 4 && m.cols >= 4, "matrix must be at least 4x4")

    MatrixCalc.slice(m,
      fromRow = 2, toRow = m.rows - 2,
      fromCol = 0, toCol = 1
    ).flatten.asArray
  }

  def outerRightEdge(m: Matrix[Int]): Array[Int] = {
    require(m.rows >= 4 && m.cols >= 4, "matrix must be at least 4x4")

    MatrixCalc.slice(m,
      fromRow = 2, toRow = m.rows - 2,
      fromCol = m.cols - 1, toCol = m.cols
    ).flatten.asArray
  }

  def outerEdges(m: Matrix[Int]): Array[Int] = {
    require(m.rows >= 4 && m.cols >= 4, "matrix must be at least 4x4")

    outerTopEdge(m) ++ outerLeftEdge(m) ++ outerRightEdge(m) ++ outerBottomEdge(m)
  }

  def innerCorners(m: Matrix[Int]): Array[Int] = {
    require(m.rows >= 4 && m.cols >= 4, "matrix must be at least 4x4")

    Array(
      m(1, 1),
      m(1, m.cols - 2),
      m(m.rows - 2, 1),
      m(m.rows - 2, m.cols - 2)
    )
  }

  def innerTopEdge(m: Matrix[Int]): Array[Int] = {
    require(m.rows >= 4 && m.cols >= 4, "matrix must be at least 4x4")

    MatrixCalc.slice(m,
      fromRow = 1, toRow = 2,
      fromCol = 2, toCol = m.cols - 2
    ).flatten.asArray
  }

  def innerBottomEdge(m: Matrix[Int]): Array[Int] = {
    require(m.rows >= 4 && m.cols >= 4, "matrix must be at least 4x4")

    MatrixCalc.slice(m,
      fromRow = m.rows - 2, toRow = m.rows - 1,
      fromCol = 2, toCol = m.cols - 2
    ).flatten.asArray
  }

  def innerLeftEdge(m: Matrix[Int]): Array[Int] = {
    require(m.rows >= 4 && m.cols >= 4, "matrix must be at least 4x4")

    MatrixCalc.slice(m,
      fromRow = 2, toRow = m.rows - 2,
      fromCol = 1, toCol = 2
    ).flatten.asArray
  }

  def innerRightEdge(m: Matrix[Int]): Array[Int] = {
    require(m.rows >= 4 && m.cols >= 4, "matrix must be at least 4x4")

    MatrixCalc.slice(m,
      fromRow = 2, toRow = m.rows - 2,
      fromCol = m.cols - 2, toCol = m.cols - 1
    ).flatten.asArray
  }

  def innerEdges(m: Matrix[Int]): Array[Int] = {
    require(m.rows >= 4 && m.cols >= 4, "matrix must be at least 4x4")

    innerTopEdge(m) ++ innerLeftEdge(m) ++ innerRightEdge(m) ++ innerBottomEdge(m)
  }

  def inner(m: Matrix[Int]): Array[Int] = {
    require(m.rows >= 4 && m.cols >= 4, "matrix must be at least 4x4")

    MatrixCalc.slice(m,
      fromRow = 2, toRow = m.rows - 2,
      fromCol = 2, toCol = m.cols - 2
    ).flatten.asArray
  }
}
