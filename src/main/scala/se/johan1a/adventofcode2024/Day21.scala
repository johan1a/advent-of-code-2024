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

  var cache = Map[(Int, Pos, Pos), Seq[Seq[Char]]]()

  def part1(input: Seq[String]): Long =

    cache = Map()

    input.map { line =>
      val code = line.toCharArray
      val sequence = shortestSequence(code)
      val c = complexity(code, sequence)
      println(s"code: $line, complexity: $c")
      c
    }.sum

  private def shortestSequence(code: Seq[Char], n: Int = 2): Seq[Char] =
    var sequences = shortestSequences(code, numpad, true)
    0.until(n).foreach { i =>
      val multiple = i < n - 1
      sequences = sequences.flatMap { shortestSequences(_, arrows, multiple) }
    }
    sequences.minBy(_.size)

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

  def shortestPaths(gridType: Int, start: Pos, end: Pos, multiple: Boolean): Seq[Seq[Char]] =
    val paths = shortestPaths(gridType, start, end)
    if multiple then
      paths
    else
      paths.take(1)

  def shortestPaths(gridType: Int, start: Pos, end: Pos): Seq[Seq[Char]] =
    if cache.contains((gridType, start, end)) then
      cache((gridType, start, end))
    else
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
      cache = cache + (((gridType, start, end)) -> result)
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

  private def complexity(code: Seq[Char], sequence: Seq[Char]): Long =
    sequence.size * numbers(code.mkString("")).head

  def part2(input: Seq[String]): Long =
    input.map { line =>
      val code = line.toCharArray
      val sequence = shortestSequence(code, n = 3)
      val c = complexity(code, sequence)
      println(s"code: $line, complexity: $c")
      c
    }.sum
