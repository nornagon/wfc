import scala.collection.mutable
import scala.util.Random
import java.io.File

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

  class OverlappedTileSet(imageFile: String, N: Int, symmetry: Int = 8) extends TileSet {
    private val (_size, _weights, _propagator, _colors, _patterns) = {
      System.setProperty("apple.awt.UIElement", "true")

      val image = javax.imageio.ImageIO.read(new File(imageFile))
      val SMX = image.getWidth
      val SMY = image.getHeight
      val colors = image.getRGB(0, 0, image.getWidth, image.getHeight, null, 0, image.getWidth).toSet.toSeq
      val sample = Array.tabulate(image.getWidth, image.getHeight) { (x, y) => colors.indexOf(image.getRGB(x, y)).toByte }
      val C = colors.size
      val W = ipow(C, N * N)

      def pattern(f: (Int, Int) => Byte): Array[Byte] = Array.tabulate(N * N) { i => f(i % N, i / N) }
      def patternFromSample(x: Int, y: Int): Array[Byte] =
        pattern { (dx, dy) => sample((x + dx) % SMX)((y + dy) % SMY) }
      def rotate(p: Array[Byte]): Array[Byte] = pattern { (x, y) => p(N - 1 - y + x * N) }
      def reflect(p: Array[Byte]): Array[Byte] = pattern { (x, y) => p(N - 1 - x + y * N) }

      def index(p: Array[Byte]): Long = {
        var result = 0l
        var power = 1l
        for (i <- p.indices) {
          result += p(p.length - 1 - i) * power
          power *= C
        }
        result
      }

      def patternFromIndex(ind: Long): Array[Byte] = {
        var residue = ind
        var power = W
        Array.tabulate(N * N) { i =>
          power /= C
          var count = 0
          while (residue >= power) {
            residue -= power
            count += 1
          }
          count.toByte
        }
      }

      val weights = mutable.Map.empty[Long, Int]
      val ordering = mutable.ArrayBuffer.empty[Long]
      for (y <- 0 until SMY - N + 1; x <- 0 until SMX - N + 1) {
        val ps = new Array[Array[Byte]](8)
        ps(0) = patternFromSample(x, y)
        ps(1) = reflect(ps(0))
        ps(2) = rotate(ps(0))
        ps(3) = reflect(ps(2))
        ps(4) = rotate(ps(2))
        ps(5) = reflect(ps(4))
        ps(6) = rotate(ps(4))
        ps(7) = reflect(ps(6))

        for (k <- 0 until symmetry) {
          val ind = index(ps(k))
          if (weights.contains(ind)) weights(ind) += 1
          else {
            weights(ind) = 1
            ordering += ind
          }
        }
      }

      val T = weights.size

      val patterns = ordering.map(patternFromIndex)
      val weights2 = ordering.map(weights)

      def agrees(p1: Array[Byte], p2: Array[Byte], dx: Int, dy: Int): Boolean = {
        val xmin = if (dx < 0) 0 else dx
        val xmax = if (dx < 0) dx + N else N
        val ymin = if (dy < 0) 0 else dy
        val ymax = if (dy < 0) dy + N else N
        for (y <- ymin until ymax; x <- xmin until xmax; if p1(x + N * y) != p2(x - dx + N * (y - dy))) return false
        true
      }

      val propagator = Array.tabulate(4, T) { (d, t) =>
        (for (t2 <- 0 until T; if agrees(patterns(t), patterns(t2), DX(d), DY(d))) yield t2)(collection.breakOut): Set[Int]
      }

      (T, weights2, propagator, colors, patterns)
    }

    override def size = _size
    override def weight(i: Int) = _weights(i)
    override def propagator(dir: Int, i: Int): Set[Int] = _propagator(dir)(i)

    def interpret(result: Array[Int], width: Int, height: Int): java.awt.image.BufferedImage = {
      val image = new java.awt.image.BufferedImage(width, height, java.awt.image.BufferedImage.TYPE_INT_RGB)
      for (y <- 0 until height) {
        val dy = if (y < height - N + 1) 0 else N - 1
        for (x <- 0 until width) {
          val dx = if (x < width - N + 1) 0 else N - 1
          val color = _colors(_patterns(result(x - dx + (y - dy) * width))(dx + dy * N))
          image.setRGB(x, y, color)
        }
      }
      image
    }
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
  def ipow(a: Int, n: Int): Long = {
    var product = 1L
    for (i <- 0 until n) product *= a
    product
  }

  private val DX = Seq(-1, 0, 1, 0)
  private val DY = Seq(0, 1, 0, -1)
  private val opposite = Seq(2, 3, 0, 1)
}
