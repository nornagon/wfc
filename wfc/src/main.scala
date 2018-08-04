import com.google.ortools.constraintsolver._

object Main {
  System.loadLibrary("jniortools")

  type TileSet = Map[String, (Seq[String], Seq[String])]

  val tileSet: TileSet = Map(
    "+" -> (
      Seq("-", "+"),
      Seq("|", "+")
    ),
    "-" -> (
      Seq("-", " ", "+"),
      Seq("-", " ")
    ),
    "|" -> (
      Seq("|", " "),
      Seq("|", " ", "+")
    ),
    " " -> (
      Seq(" ", "|"),
      Seq(" ", "-")
    )
  )

  val ell: TileSet = Map(
    "┌" -> (
      Seq("┘", "┐"),
      Seq("┘", "└")
    ),
    "└" -> (
      Seq("┘", "┐"),
      Seq(" ", "┐", "┌")
    ),
    "┘" -> (
      Seq(" ", "└", "┌"),
      Seq(" ", "┌", "┐")
    ),
    "┐" -> (
      Seq(" ", "└", "┌"),
      Seq("┘", "└")
    ),
    " " -> (
      Seq(" ", "└", "┌"),
      Seq(" ", "┌", "┐")
    )
  )

  def wfc(ts: TileSet, size: (Int, Int), seed: Int = 0, trace: Boolean = false): Array[Array[Int]] = {
    val params = ConstraintSolverParameters.newBuilder()
      .mergeFrom(Solver.defaultSolverParameters())
      .setTraceSearch(false)
      .build()
    val solver = new Solver("WFC", params)
    solver.reSeed(seed)
    val horizontallyAllowedPairs = new IntTupleSet(2)
    val verticallyAllowedPairs = new IntTupleSet(2)
    val indexTiles = ts.keys.toSeq
    val tileIndices = indexTiles.zipWithIndex.toMap
    for ((t, (hNeighbors, vNeighbors)) <- ts) {
      val ti = tileIndices(t)
      for (hn <- hNeighbors) {
        val hni = tileIndices(hn)
        horizontallyAllowedPairs.insert2(ti, hni)
      }
      for (vn <- vNeighbors) {
        val vni = tileIndices(vn)
        verticallyAllowedPairs.insert2(ti, vni)
      }
    }

    val grid = Array.fill(size._1, size._2) { solver.makeIntVar(0, ts.size) }

    for (y <- grid.indices) {
      val row = grid(y)
      for (x <- row.indices) {
        row(x).setName(s"[$x,$y]")
        if (x < row.length - 1) {
          val l = row(x)
          val r = row(x + 1)
          solver.addConstraint(solver.makeAllowedAssignment(Array(l, r), horizontallyAllowedPairs))
        }
        if (y < grid.length - 1) {
          val t = row(x)
          val b = grid(y+1)(x)
          solver.addConstraint(solver.makeAllowedAssignment(Array(t, b), verticallyAllowedPairs))
        }
      }
    }

    val db = solver.makePhase(grid.flatten,
      Solver.CHOOSE_MIN_SIZE,
      Solver.ASSIGN_RANDOM_VALUE)
    if (trace) {
      val st = new SearchMonitor(solver) {
        override def applyDecision(decision: Decision): Unit = {
          for (y <- grid.indices) {
            val row = grid(y)
            for (x <- row.indices) {
              if (grid(y)(x).bound()) {
                print(indexTiles(grid(y)(x).value().toInt))
              } else {
                val domainSize = grid(y)(x).Size().intValue()
                print(if (domainSize < 10) domainSize else "?")
              }
            }
            println()
          }
          println()
        }
      }
      solver.newSearch(db, st)
    } else {
      solver.newSearch(db)
    }
    if (!solver.nextSolution()) {
      throw new RuntimeException("No solution found")
    }
    val ret = grid.map(row => row.map(v => v.value().toInt))
    solver.endSearch()
    ret
  }

  /*
  def solve(): Unit = {
    val params = ConstraintSolverParameters.newBuilder()
      .mergeFrom(Solver.defaultSolverParameters())
      .setTraceSearch(false)
      .build()
    val solver = new Solver("wfc", params)

    val itsHoriz = new IntTupleSet(2)
    for ((t, allowedNeighbors) <- validPairsHoriz; n <- allowedNeighbors) {
      itsHoriz.insert2(tiles.indexOf(t), tiles.indexOf(n))
      itsHoriz.insert2(tiles.indexOf(n), tiles.indexOf(t))
    }
    val itsVert = new IntTupleSet(2)
    for ((t, allowedNeighbors) <- validPairsVert; n <- allowedNeighbors) {
      itsVert.insert2(tiles.indexOf(t), tiles.indexOf(n))
      itsVert.insert2(tiles.indexOf(n), tiles.indexOf(t))
    }

    val grid = Array.fill(8, 8) { solver.makeIntVar(0, tiles.size) }

    for (y <- grid.indices) {
      val row = grid(y)
      for (x <- row.indices) {
        row(x).setName(s"[$x,$y]")
        if (x < row.length - 1) {
          val l = row(x)
          val r = row(x + 1)
          solver.addConstraint(solver.makeAllowedAssignment(Array(l, r), itsHoriz))
        }
        if (y < grid.length - 1) {
          val t = row(x)
          val b = grid(y+1)(x)
          solver.addConstraint(solver.makeAllowedAssignment(Array(t, b), itsVert))
        }
      }
    }

    //solver.addConstraint(solver.makeNonEquality(iva, ivb))
    val db = solver.makePhase(grid.flatten,
      Solver.CHOOSE_MIN_SIZE,
      Solver.ASSIGN_RANDOM_VALUE)
    solver.reSeed(1)
    val log = solver.makeSearchLog(/* branch_count */ 100)
    val limit = solver.makeLimit(
      SearchLimitParameters.newBuilder()
        .mergeFrom(solver.makeDefaultSearchLimitParameters())
        .setSolutions(1)
        .build())
    val st = new SearchMonitor(solver) {
      override def applyDecision(decision: Decision): Unit = {
        for (y <- grid.indices) {
          val row = grid(y)
          for (x <- row.indices) {
            if (grid(y)(x).bound()) {
              print(tiles(grid(y)(x).value().toInt))
            } else {
              val domainSize = grid(y)(x).Size().intValue()
              print(if (domainSize < 10) domainSize else "?")
            }
          }
          println()
        }
        println()
      }
    }
    solver.newSearch(db, log, limit, st)
    while (solver.nextSolution()) {
      for (y <- grid.indices) {
        val row = grid(y)
        for (x <- row.indices) {
          print(tiles(grid(y)(x).value().toInt))
        }
        println()
      }
    }
    solver.endSearch()
    println(s"solns: ${solver.solutions()}")
  }
  */

  def printGrid(ts: TileSet, grid: Array[Array[Int]]): Unit = {
    val tsk = ts.keys.toSeq
    for (row <- grid) {
      for (cell <- row) {
        print(tsk(cell))
      }
      println()
    }
  }

  def main(args: Array[String]): Unit = {
    printGrid(ell, wfc(ell, (8, 8), seed=1209))
  }
}
