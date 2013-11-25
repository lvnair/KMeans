package edu.cs.umass.Kmeans

import java.util.Random
import cc.factorie.la.{DenseTensor1, Tensor1, Tensor}

/**
 * Created by IntelliJ IDEA.
 * User: apassos
 * Date: 1/25/12
 * Time: 11:22 AM
 * To change this template use File | Settings | File Templates.
 */

/**
 * An implementation of kmeans using parallel collections
 * @param data The data points to be clustered
 * @param k The number of clusters
 * @param d The dimensionality of the data
 */
class ParallelKMeans(data: Seq[Tensor1], k: Int,  d: Int, initialMeans: Seq[DenseTensor1] = null) {

  val means = Array.fill[DenseTensor1](k)(null)
  val norms = Array.fill(k)(0.0)
  var points = Array.fill[(Int,Double)](data(0).length)(null)
  val rng = new java.util.Random()

  // (x-y)^2 = x^2 - 2xy + y^2
  // note that x^2 is constant, so we can ignore it
  def dist(d: Tensor1, m: Int) = norms(m)  - 2* d.dot(means(m))

  def initialize() {
    for (i <- 0 until k) {
      if (initialMeans ne null)
        means(i) = initialMeans(i)
      else {
        means(i) = new DenseTensor1(d)
        means(i) += data(rng.nextInt(data.length))
      }
      norms(i) = means(i).twoNormSquared
    }
  }



  def eStep() = {
    cc.factorie.util.Threading.parMap(data, Runtime.getRuntime.availableProcessors())
      {
       d =>
      (0 until k).map(i => (i,  dist(d, i))).minBy(a => a._2)
    }.toSeq
  }

  def mStep(e: Seq[(Int,Double)]) = {
    //println("means "+means.toSeq)
    //println("points "+e)
    points = e.toArray
    //println(e.zipWithIndex.par.groupBy(f => f._1._1))
    //println(e.zipWithIndex.par.groupBy(f => f._1._1).map(group => {println(group._2)}))
    e.zipWithIndex.par.groupBy(f => f._1._1).map(group => {
      val mean = group._1
      //println("mean "+mean)
      val examples = group._2
      //println("examples "+examples)
      val newMean = means(mean)
      //println("new mean "+newMean)
      newMean.zero()
      //println("newmean zero "+newMean.zero())
      examples.foreach(e => newMean += data(e._2))
      val error = examples.map(_._1._2).sum
      newMean /= examples.length
      norms(mean) = newMean.twoNormSquared
      error
    }).sum
  }

  def process(iterations: Int, tolerance: Double = 0.001) = {
    var err = 1000000000.0
    var oldError = Double.PositiveInfinity
    var i = 0
    while (i < iterations && oldError - err > tolerance) {
      oldError = err

      err = mStep(eStep())
      i += 1
    }
    err
  }

}

object ParallelKMeans {
  def goodInitialize(data: Seq[Tensor1], k: Int, d: Int, iterations: Int, f: Double) = {
    val rng = new scala.util.Random(0)
    val initialMeans = rng.shuffle(StreamingKMeans.process(data, f)).take(k).toSeq
    val km = new ParallelKMeans(data, k, d, initialMeans)
    km.initialize()
    km.process(iterations)
    km.points

  }

  // Runs the parallel KMeans n times and returns the means of the best run
  def bestOfN(data: Seq[Tensor1], k: Int,  n: Int,  d: Int, iterations: Int) = {
    var bestKM: ParallelKMeans = null
    var bestErr = Double.PositiveInfinity
    for (i <- 0 until n) {
      val km = new ParallelKMeans(data, k, d)
      km.initialize()
      val newerr = km.process(iterations)
      if (newerr < bestErr) {
        bestKM = km
        bestErr = newerr
      }
    }
    println("Best mean "+bestKM.means.toSeq)
    //bestKM.means
    bestKM.points
  }
}