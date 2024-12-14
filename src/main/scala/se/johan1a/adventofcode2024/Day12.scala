package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024
import se.johan1a.adventofcode2024.Utils.*

object Day12:

  private var seen = Set[Pos]()

  def part1(input: Seq[String]): Int =
    val grid = makeGrid(input)
    seen = Set[Pos]()
    allPositions(grid).filterNot(seen.contains).map(pos =>
      val (area, perimiter, nbrPerimiter, _) = calculate(grid, pos)
      area * nbrPerimiter
    ).sum

  private def calculate(grid: Utils.Grid, start: Vec2): (Int, Set[Pos], Int, Set[Pos]) =
    var queue = Seq(start)
    var area = 0
    var perimiter = Set[Pos]()
    var nbrPerimiter = 0
    val plantType = get(grid, start)
    var localGroup = Set[Pos]()
    while queue.nonEmpty do
      val pos = queue.head
      queue = queue.tail
      if !seen.contains(pos) then
        seen = seen + pos
        localGroup = localGroup + pos
        area += 1

        val below = Vec2(0, 1) + pos
        val above = Vec2(0, -1) + pos
        val rightOf = Vec2(1, 0) + pos
        val leftOf = Vec2(-1, 0) + pos

        val neighbors = Seq(below, above, rightOf, leftOf)
        neighbors.foreach(neighbor =>
          if getOpt(grid, neighbor).contains(plantType) then
            queue = queue :+ neighbor
          else if isPerimiter(grid, plantType, neighbor) then
            perimiter = perimiter + neighbor
            nbrPerimiter += 1
        )
    (area, perimiter, nbrPerimiter, localGroup)

  private def isPerimiter(grid: Grid, plantType: Char, neighbor: Pos) =
    !inRange(grid, neighbor) || get(grid, neighbor) != plantType

  def part2(input: Seq[String]): Int =
    val grid = makeGrid(input)
    seen = Set[Pos]()
    allPositions(grid).filterNot(seen.contains).map(pos =>
      // also for each hole, go around it inside. keep track of visited holes
      // add outline to hole nbr sides
      val (area, perimiter, _, group) = calculate(grid, pos)
      if area > 0 then

        val topLeft = group.toSeq.minBy(p => (p.y, p.x))
        val plantType = get(grid, pos)
        val nbrSides = getNbrSides(grid, perimiter, plantType)

        area * nbrSides
      else
        0
    ).sum

  private def getNbrSides(grid: Grid, perimiter: Set[Pos], plantType: Char) =
    var seen = Set[(Pos, Dir)]()
    var nbrSides = 0
    var first = true
    perimiter
      .toSeq
      .sortBy(p => (p.y, p.x))
      .foreach { start =>
        if !seen.map(_._1).contains(start) then
          var pos = start
          var dir = Left
          if first then
            dir = Right
            first = false
          while !seen.contains((pos, dir)) do
            seen = seen + ((pos, dir))

            val nextPosInFront = pos + dir
            val nextPosToTheRight = getNextToTheRight(grid, pos, dir)
            val nextInFront = perimiter.contains(nextPosInFront)
            val nextRight = perimiter.contains(nextPosToTheRight)
            val rightOf = perimiter.contains(getPosToTheRight(grid, pos, dir))

            (nextInFront, nextRight, rightOf) match
              case (_, true, false) =>
                pos = pos + dir
                dir = turnRight(dir)
                pos = pos + dir
                nbrSides += 1
              case (_, _, true) =>
                nbrSides += 1
                dir = turnRight(dir)
              case (false, false, false) =>
                dir = turnLeft(dir)
                nbrSides += 1
              case (_, false, _) =>
                pos = pos + dir
      }
    nbrSides

  private def getSame(grid: Grid, originalPlantType: Char, pos: Vec2) =
    getOpt(grid, pos) match
      case Some(plantType) if plantType == originalPlantType => Some(plantType)
      case _                                                 => None

  private def getNextToTheRight(grid: Grid, pos: Pos, dir: Dir) =
    val inFront = pos + dir
    val dirToTheRight = turnRight(dir)
    inFront + dirToTheRight

  private def getPosToTheLeft(grid: Grid, pos: Pos, dir: Dir) =
    val dirToTheLeft = turnLeft(dir)
    pos + dirToTheLeft

  private def getPosToTheRight(grid: Grid, pos: Pos, dir: Dir) =
    val dirToTheRight = turnRight(dir)
    pos + dirToTheRight
