package image_signature

import com.letstalkdata.scalinear.{Matrix, Vector}
import sun.reflect.generics.reflectiveObjects.NotImplementedException
import scala.reflect.ClassTag

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
  def sum(m: Matrix[Int], axis: Int): Vector[Int] = {
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

  def slice(m: Matrix[Int], fromRow: Int, toRow: Int, fromCol: Int, toCol: Int): Matrix[Int] = {
    require(fromRow < m.rows + 1 && toRow < m.rows + 1)
    require(fromCol < m.cols + 1 && toCol < m.cols + 1)
    val rows = Matrix(m.asArray.slice(fromRow, toRow):_*)
    Matrix(rows.t[Int].asArray.slice(fromCol, toCol):_*).t[Int]
  }
}
