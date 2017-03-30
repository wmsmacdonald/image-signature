package image_signature

import com.letstalkdata.scalinear.{Matrix, Vector}

object MatrixCalc {
  // https://github.com/numpy/numpy/blob/v1.10.1/numpy/lib/function_base.py#L1116-L1175
  def diff(m: Matrix[Int], axis: Int): Matrix[Int] = {
    if (axis == 0) {
      val pairs = m.asArray.sliding(2, 1)
      Matrix(pairs.map(vectors => vectors(1) - vectors(0)).toSeq:_*)
    }
    else if (axis == 1) {
      Matrix(m.asArray.map((v: Vector[Int]) => diff(v)).toSeq:_*)
    }
    else {
      throw new NotImplementedError()
    }
  }

  def diff(v: Vector[Int]): Vector[Int] = {
    Vector(v.asArray.sliding(2, 1).map(pair => pair(1) - pair(0)).toSeq:_*)
  }

  def absolute(m: Matrix[Int]): Matrix[Int] = {
    Matrix(m.asArray.map(v => Vector(v.asArray.map(el => math.abs(el)):_*)):_*)
  }

  // https://github.com/numpy/numpy/blob/v1.12.0/numpy/core/fromnumeric.py#L1710-L1814
  def sum(m: Matrix[Int], axis: Int = -1): Vector[Int] = {
    if (axis == -1) {
      return Vector(m.asArray.map(v => sum(v)).sum)
    }
    if (axis == 0) {
      val len = m.asArray(0).length
      m.asArray.fold(Vector.zeros[Int](len))((prev: Vector[Int], curr: Vector[Int]) => prev + curr)
    }
    else if (axis == 1) {
      Vector(m.asArray.map((v: Vector[Int]) => sum(v)):_*)
    }
    else {
      throw new NotImplementedError()
    }
  }

  def sum(v: Vector[Int]): Int = v.asArray.sum

  // https://github.com/numpy/numpy/blob/v1.12.0/numpy/core/fromnumeric.py#L2033-L2097
  def cumsum(m: Matrix[Int], axis: Int = -1): Matrix[Int] = {
    if (axis == -1) {
      Matrix(cumsum(m.flatten))
    }
    else if (axis == 0) {
      val len = m.asArray(0).length
      val z = Vector.zeros[Int](len)
      val rows: Array[Vector[Int]] = m.asArray.scanLeft(z)((r: Vector[Int], el: Vector[Int]) => r + el).tail
      Matrix(rows:_*)
    }
    else if (axis == 1) {
      cumsum(m.t[Int], 0).t[Int]
    }
    else {
      throw new NotImplementedError()
    }
  }

  def cumsum(v: Vector[Int]): Vector[Int] = {
    Vector(v.asArray.scanLeft(0)(_ + _).tail:_*)
  }

  // TODO use binary search
  def searchsorted(v: Vector[Int], value: Int): Int = {
    val i = v.asArray.indexWhere(_ > value)
    if (i == -1) v.length else i
  }

  // grab rectangular section from matrix using rows
  def slice(m: Matrix[Int], fromRow: Int, toRow: Int): Matrix[Int] = {
    Matrix(m.asArray.slice(fromRow, toRow):_*)
  }

  // grab rectangular section from matrix using both rows and columns
  // slice fromRow (inclusive) toRow (exclusive) and fromCol (inclusive) toCol (exclusive)
  def slice(m: Matrix[Int], fromRow: Int, toRow: Int, fromCol: Int, toCol: Int): Matrix[Int] = {
    slice(
      // select rows and then transpose
      slice(m, fromRow, toRow).t[Int],
      // select columns and transpose back
      fromCol, toCol).t[Int]
  }
}
