package com.gabbi.profiling

/**
 * Created by gabbi on 16/08/14.
 */
object PerformanceComputation {

  case class Stats[T](result: T, averageTime: Double, stdDev: Double) {
    override def toString = s"function computes result: +$result " +
      s"in $averageTime ms on average with standard deviation $stdDev"
  }

  def getStats[O](function: => O, numRuns: Int = 5): Stats[O] = {
    assert(numRuns > 1)

    def compute: (O, Double) = {
      val t1 = System.nanoTime().toDouble
      val res = function
      val t2 = System.nanoTime().toDouble
      (res, (t2 - t1) / 1e6)
    }

    val (results, times) = (1 to numRuns + 5).map(x => compute).drop(5).unzip

    val avgTime = times.reduceLeft(_ + _) / times.size

    val stdDev = standardDeviation(avgTime, times)

    Stats(results.head, avgTime, stdDev)
  }

  def standardDeviation(average: Double, list: Seq[Double]) = list match {
    case Nil => 0.0
    case _ =>
      math.sqrt(list.foldLeft(0.0) { case (acc, x) => acc + math.pow(x - average, 2)} / list.length)

  }

  def publishPerformanceData[O](function: => O, numRuns: Int = 5): Unit = {
    val stats = getStats(function, numRuns)
    println(stats)
  }
}
