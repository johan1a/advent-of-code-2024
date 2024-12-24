package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024.Utils.*

object Day21:

  val numpad = 0
  val arrows = 1

  private val numpadGrid =
    makeGrid(
      """#####
      |#789#
      |#456#
      |#123#
      |##0A#
      |#####""".stripMargin.split("\n")
    )

  private val arrowsGrid = makeGrid(
    """#####
      |##^A#
      |#<v>#
      |#####
      |""".stripMargin.split("\n")
  )

  private var scoreCache = Map[(Int, Vec2, Vec2), Long]()
  private var pathCache = Map[(Int, Pos, Pos), Seq[Seq[Char]]]()

  def part1(input: Seq[String], n: Int = 2): Long =
    input.map { line =>
      val code = line.toCharArray
      val cost = topCost(code, n)
      complexity(code, cost)
    }.sum

  def part2(input: Seq[String]): Long = part1(input, 25)

  def topCost(code: Seq[Char], n: Int): Long =
    val sequences = shortestSequences(code, numpad)
    val topCosts = shortestSequences(code, numpad).map { sequence =>
      val modifiedSequence = 'A' +: sequence
      modifiedSequence.sliding(2).toSeq.map { pair =>
        cost(pair.head, pair.last, n)
      }.sum
    }
    topCosts.min

  def cost(a: Char, b: Char, n: Int): Long =
    cost(charToPos(a), charToPos(b), n)

  private def charToPos(char: Char): Vec2 =
    find(arrowsGrid, char).get

  def cost(a: Vec2, b: Vec2, n: Int): Long =
    val key = (n, a, b)
    if scoreCache.contains(key) then
      scoreCache(key)
    else
      val results: Seq[Long] = shortestPathsCached(arrows, a, b).map { path =>
        val sequence = 'A' +: path :+ 'A'
        if n == 1 then
          sequence.size - 1
        else
          val pairs = sequence.sliding(2).toSeq
          val result = pairs.map(pair =>
            cost(pair.head, pair.last, n - 1)
          ).sum
          result
      }
      val best = results.min
      scoreCache = scoreCache + (key -> best)
      best

  def shortestSequences(code: Seq[Char], gridType: Int): Seq[Seq[Char]] =
    val grid = if gridType == numpad then numpadGrid else arrowsGrid
    var pos = find(grid, 'A').get
    var sequences = Seq[Seq[Char]]()
    code.foreach { targetChar =>
      val target = find(grid, targetChar).get
      val paths = shortestPaths(gridType, pos, target)
      val pathsWithA = paths.map(s => s :+ 'A')
      sequences = pathsWithA.flatMap(path =>
        if sequences.isEmpty then
          Seq(path)
        else
          sequences.map(sequence => sequence ++ path)
      )
      pos = target
    }
    sequences

  private def shortestPathsCached(gridType: Int, start: Pos, end: Pos): Seq[Seq[Char]] =
    val state = (gridType, start, end)
    if pathCache.contains(state) then
      pathCache(state)
    else
      val paths = shortestPaths(gridType, start, end)
      pathCache = pathCache + (state -> paths)
      paths

  private def score(path: Seq[Char]) =
    var i = 1
    var score = 0
    while i < path.length do
      if path(i) != path(i - 1) then
        score = score - 1
      i += 1
    score

  def shortestPaths(gridType: Int, start: Pos, end: Pos): Seq[Seq[Char]] =
    val grid = if gridType == numpad then numpadGrid else arrowsGrid
    var queue = Seq(start)
    var found = false
    var seen = Set[Pos]()
    var prev = Map[Pos, Seq[Pos]]()
    var dist = Map[Pos, Int](start -> 0).withDefaultValue(Int.MaxValue / 2)

    while queue.nonEmpty do
      val pos = queue.head
      queue = queue.tail
      if !seen.contains(pos) then
        seen = seen + pos

        if pos == end then
          found = true

        neighbors(pos, includeDiagonals = false)
          .filter(p => inRange(grid, p) && get(grid, p) != '#')
          .foreach { neighbor =>
            if dist(pos) + 1 <= dist(neighbor) then
              dist = dist + (neighbor -> (dist(pos) + 1))
              prev = prev + (neighbor -> (prev.getOrElse(neighbor, Seq.empty) :+ pos))
              queue = queue :+ neighbor
          }
    val path = getPath(prev, end)
    path.map(getCharSequence)

  private def getCharSequence(sequence: Seq[Pos]): Seq[Char] =
    if sequence.size < 2 then
      Seq.empty
    else
      sequence.sliding(2).toSeq.map(ab =>
        val a = ab.head
        val b = ab.last
        charDir(a, b)
      )

  private def getPath(prev: Map[Pos, Seq[Pos]], pos: Pos): Seq[Seq[Pos]] =
    prev.getOrElse(pos, Seq.empty) match
      case Seq()             => Seq(Seq(pos))
      case befores: Seq[Pos] => befores.flatMap(before => getPath(prev, before).map(_ :+ pos))

  private def charDir(a: Pos, b: Pos) =
    if a leftOf b then
      '>'
    else if a rightOf b then
      '<'
    else if a above b then
      'v'
    else if a below b then
      '^'
    else ???

  private def complexity(code: Seq[Char], sequenceLength: Long): Long =
    sequenceLength * numbers(code.mkString("")).head
