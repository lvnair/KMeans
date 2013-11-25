package edu.cs.umass
import cc.factorie.la._

/**
 * Created with IntelliJ IDEA.
 * User: lvnair
 * Date: 11/17/13
 * Time: 6:26 PM
 * To change this template use File | Settings | File Templates.
 */
object Test {
  def main(args:Array[String]) {
    val dt = new DenseTensor1(5) // Creates a vector of length 5, internally stored as an Array[Double]
    dt(0) = 1.2
    println("Tensor dt is "+dt)
    val st = new SparseTensor1(999999) // Creates a vector of length 999999, but which efficiently stores only its non-zero values
    st(555) = 2.3
    println(st)

    println("Tensor st oneNorm is "+st.oneNorm)

    val ut = new UniformTensor1(33, 0.1) // A vector of length 33, in which all values are 0.1

  }
}
