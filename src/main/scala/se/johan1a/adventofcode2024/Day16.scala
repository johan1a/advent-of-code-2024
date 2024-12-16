package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024.Utils.*

import scala.collection.mutable

object Day16:

  def part1(input: Seq[String]): Int =
    val grid = makeGrid(input)
    val start = find(grid, 'S').get
    val end = find(grid, 'E').get
    shortestPath(grid, start, end)._1

  private def shortestPath(grid: Grid, start: Pos, end: Pos) =
    var cost = Map[(Pos, Dir), Int]((start, Right) -> 0)
    val toVisit = new mutable.PriorityQueue[(Pos, Dir)]()(Ordering.by((p, _) => -manhattan(p, end)))
    toVisit += ((start, Right))
    var best = Int.MaxValue
    var prev = Map[(Pos, Dir), Set[(Pos, Dir)]]()
    while toVisit.nonEmpty do
      val (pos, dir) = toVisit.dequeue()

      if pos == end then
        best = Math.min(best, cost((pos, dir)))

      Seq(
        (pos + dir, dir, 1),
        (pos + turnLeft(dir), turnLeft(dir), 1001),
        (pos + turnRight(dir), turnRight(dir), 1001)
      ).foreach { (neighbor, newDir, extraCost) =>
        if get(grid, neighbor) != '#' then
          val c = cost.get((pos, dir)).map(c => c + extraCost).getOrElse(Int.MaxValue)
          val neighborState = (neighbor, newDir)
          val prevCost = cost.getOrElse(neighborState, Int.MaxValue)
          if c <= prevCost && c <= best then
            if c == prevCost then
              prev = prev + (neighborState -> (prev.getOrElse(neighborState, Set.empty) + ((pos, dir))))
            else if c < prevCost then
              prev = prev + (neighborState -> Set((pos, dir)))

            cost = cost + (neighborState -> c)
            toVisit += neighborState
      }
    (best, cost, prev)

  def part2(input: Seq[String]): Int =
    val grid = makeGrid(input)
    val start = find(grid, 'S').get
    val end = find(grid, 'E').get
    val (best, cost, prev) = shortestPath(grid, start, end)
    var positions = Set[Pos]()
    var queue: Set[(Pos, Dir)] =
      Set(Left, Right, Up, Down).filter { d => prev.contains((end, d)) && cost((end, d)) == best }.map(d => (end, d))
    while queue.nonEmpty do
      val (pos, dir) = queue.head
      val state = (pos, dir)
      queue = queue - state
      val before = prev.getOrElse(state, Set.empty)
      queue = queue ++ before
      positions = positions ++ before.map(_._1)

//    positions.foreach(p =>
//      set(grid, p, 'O')
//    )
    // printGrid(grid)
    positions.size + 1

  def getPath(prev: Map[Pos, Set[Pos]], pos: Pos): Set[Pos] =
    prev.get(pos) match
      case None => Set.empty
      case Some(previous) =>
        previous.flatMap { p => getPath(prev, p) } + pos
