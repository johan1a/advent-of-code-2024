package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024.Utils.*

object Day21:

  val numpad = 0
  val arrows = 1

  val numpadGrid =
    makeGrid(
      """#####
      |#789#
      |#456#
      |#123#
      |##0A#
      |#####""".stripMargin.split("\n")
    )

  val arrowsGrid = makeGrid(
    """#####
      |##^A#
      |#<v>#
      |#####
      |""".stripMargin.split("\n")
  )

  var cache = Map[(Int, Vec2, Vec2), Long]()

  def part1(input: Seq[String]): Long =

    cache = Map()

    input.map { line =>
      val code = line.toCharArray
      val bottomLength = topCost(code)
      val c = complexity(code, bottomLength)
      println(s"code: $line, complexity: $c, length: $bottomLength")
      c
    }.sum

  def topCost(code: Seq[Char], n: Int = 2): Long =
    val sequences = shortestSequences(code, numpad, true)
    shortestSequences(code, numpad, true).map { sequence =>
      val modifiedSequence = 'A' +: sequence
      val pairs = modifiedSequence.sliding(2).toSeq
      val costs = pairs.map { pair =>
        cost(pair.head, pair.last, n)
      }
      costs.sum
    }.min

  private def charToPos(char: Char): Vec2 =
    // TODO cache
    find(arrowsGrid, char).get

  def cost(a: Char, b: Char, n: Int): Long =
    cost(charToPos(a), charToPos(b), n)

  def cost(a: Vec2, b: Vec2, n: Int): Long =
    val key = (n, a, b)
    if cache.contains(key) then
      cache(key)
    else
      val sequence = 'A' +: shortestPath(arrows, a, b, true) :+ 'A'
      if n == 1 then
        // TODO check
        val result = sequence.size - 1
        println(s"  1: $result")
        result
      else
        val pairs = sequence.sliding(2).toSeq
        val result = pairs.map(pair =>
          cost(pair.head, pair.last, n - 1)
        ).sum
        cache = cache + (key -> result)
        println(s"$n: $result")
        result

  def shortestSequences(code: Seq[Char], gridType: Int, multiple: Boolean): Seq[Seq[Char]] =
    val grid = if gridType == numpad then numpadGrid else arrowsGrid
    var pos = find(grid, 'A').get
    var sequences = Seq[Seq[Char]]()
    code.foreach { targetChar =>
      val target = find(grid, targetChar).get
      val paths = shortestPaths(gridType, pos, target, multiple)
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

  // todo cache
  def shortestPath(gridType: Int, start: Pos, end: Pos, multiple: Boolean): Seq[Char] =
    val paths = shortestPaths(gridType, start, end, true)
    paths.maxBy(p => score(p))

  def score(path: Seq[Char]) =
    var i = 1
    var score = 0
    while i < path.length do
      if path(i) != path(i - 1) then
        score = score - 1
      i += 1
    score

  def shortestPaths(gridType: Int, start: Pos, end: Pos, multiple: Boolean): Seq[Seq[Char]] =
    val paths = shortestPaths(gridType, start, end)
    if multiple then
      paths
    else
      paths.take(1)

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
    val sequences = getSequences(prev, end)
    val result = sequences.map(getCharSequence)
    result

  private def getCharSequence(sequence: Seq[Pos]): Seq[Char] =
    if sequence.size < 2 then
      Seq.empty
    else
      sequence.sliding(2).toSeq.map(ab =>
        val a = ab.head
        val b = ab.last
        charDir(a, b)
      )

  private def getSequences(prev: Map[Pos, Seq[Pos]], pos: Pos): Seq[Seq[Pos]] =
    prev.getOrElse(pos, Seq.empty) match
      case Seq()             => Seq(Seq(pos))
      case befores: Seq[Pos] => befores.flatMap(before => getSequences(prev, before).map(_ :+ pos))

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

  def part2(input: Seq[String]): Long =
    input.map { line =>
      val code = line.toCharArray
      val sequence = topCost(code, n = 3)
      val c = complexity(code, sequence)
      println(s"code: $line, complexity: $c")
      c
    }.sum
