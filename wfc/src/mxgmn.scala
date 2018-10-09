import scala.collection.mutable
import scala.util.Random

object mxgmn {
  trait TileSet {
    def size: Int

    def weight(i: Int): Double

    /**
      * @param dir direction of propagation
      * @param i tile to propagate from
      * @return set of allowable tiles from tile i in direction dir
      */
    def propagator(dir: Int, i: Int): Set[Int]
  }

  class Model(width: Int, height: Int, tiles: TileSet)(implicit random: Random) {
    private val wave = Array.fill(width * height, tiles.size)(true)
    private val compatible = Array.fill(width * height, tiles.size, 4)(0)

    private val weightLogWeights = Array.tabulate(tiles.size)(i => tiles.weight(i) * math.log(tiles.weight(i)))
    private val sumOfWeights = (0 until tiles.size).foldLeft(0d) { (m, o) => m + tiles.weight(o) }
    private val sumOfWeightLogWeights = weightLogWeights.sum
    private val startingEntropy = math.log(sumOfWeights) - sumOfWeightLogWeights / sumOfWeights

    private val sumsOfOnes = Array.fill(width * height)(0)
    private val sumsOfWeights = Array.fill(width * height)(0d)
    private val sumsOfWeightLogWeights = Array.fill(width * height)(0d)
    private val entropies = Array.fill(width * height)(0d)

    private val stack = mutable.ArrayBuffer.empty[(Int, Int)]

    private var observed: Array[Int] = _

    def result: Option[Array[Int]] = Option(observed)

    private def observe(): Option[Boolean] = {
      var min = 1e3
      var argmin = -1

      for (i <- wave.indices) {
        if (!onBoundary(i % width, i / width)) {
          val amount = sumsOfOnes(i)
          if (amount == 0) return Some(false)
          val entropy = entropies(i)
          if (amount > 1 && entropy <= min) {
            val noise = 1e-6 * random.nextDouble()
            if (entropy + noise < min) {
              min = entropy + noise
              argmin = i
            }
          }
        }
      }

      if (argmin == -1) {
        observed = Array.tabulate(width * height) { i => wave(i).indexOf(true) }
        return Some(true)
      }
      val distribution = Array.tabulate(tiles.size) { i => if (wave(argmin)(i)) tiles.weight(i) else 0 }
      val r = randomFromDistribution(distribution, random.nextDouble())
      val w = wave(argmin)
      for (t <- 0 until tiles.size) if (w(t) != (t == r)) ban(argmin, t)

      None
    }

    private val DX = Seq(-1, 0, 1, 0)
    private val DY = Seq(0, 1, 0, -1)
    private val opposite = Seq(2, 3, 0, 1)

    private def onBoundary(x: Int, y: Int): Boolean = x < 0 || y < 0 || x >= width || y >= height

    private def propagate(): Unit = {
      while (stack.nonEmpty) {
        val e1 = stack.remove(stack.size - 1)
        val i1 = e1._1
        val x1 = i1 % width
        val y1 = i1 / width
        for (d <- 0 until 4) {
          val x2 = x1 + DX(d)
          val y2 = y1 + DY(d)

          if (!onBoundary(x2, y2)) {
            val i2 = x2 + y2 * width
            val p = tiles.propagator(d, e1._2)
            val compat = compatible(i2)

            p.foreach { t2 =>
              val comp = compat(t2)
              comp(d) -= 1
              if (comp(d) == 0) ban(i2, t2)
            }
          }
        }
      }
    }

    def run(limit: Int): Boolean = {
      clear()
      var l = 0
      while (l < limit || limit == 0) {
        observe() match {
          case Some(r) => return r
          case None => propagate()
        }
        l += 1
      }
      true
    }

    private def ban(i: Int, t: Int): Unit = {
      wave(i)(t) = false
      val comp = compatible(i)(t)
      comp.indices.foreach { d => comp(d) = 0 }
      stack += ((i, t))
      val sum = sumsOfWeights(i)
      entropies(i) += sumsOfWeightLogWeights(i) / sum - math.log(sum)

      sumsOfOnes(i) -= 1
      sumsOfWeights(i) -= tiles.weight(t)
      sumsOfWeightLogWeights(i) -= weightLogWeights(t)

      val sum2 = sumsOfWeights(i)
      entropies(i) -= sumsOfWeightLogWeights(i) / sum2 - math.log(sum2)
    }

    private def clear(): Unit = {
      for (i <- wave.indices) {
        for (t <- 0 until tiles.size) {
          wave(i)(t) = true
          for (d <- 0 until 4) {
            compatible(i)(t)(d) = tiles.propagator(opposite(d), t).size
          }
        }

        sumsOfOnes(i) = tiles.size
        sumsOfWeights(i) = sumOfWeights
        sumsOfWeightLogWeights(i) = sumOfWeightLogWeights
        entropies(i) = startingEntropy
      }
    }
  }

  def randomFromDistribution(a: Array[Double], r: Double): Int = {
    var sum = a.sum
    if (sum == 0) {
      a.indices.foreach(i => a(i) = 1)
      sum = a.length
    }
    a.indices.foreach(i => a(i) /= sum)

    var i = 0
    var x = 0d
    while (i < a.length) {
      x += a(i)
      if (r <= x) return i
      i += 1
    }

    0
  }
}
