package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024.Utils.*

object Day08:

  case class Antenna(pos: Vec2, frequency: Char)

  def part1(input: Seq[String]): Int =
    val (grid, antennas) = parse(input)
    getAntiNodes(grid, antennas).size

  def part2(input: Seq[String]): Int =
    val (grid, antennas) = parse(input)
    getAntiNodes2(grid, antennas).size

  private def getAntiNodes(grid: Grid, antennas: Seq[Antenna]): Set[Vec2] =
    val types = antennas.map(_.frequency).distinct
    var antinodes = Set[Vec2]()
    types.foreach { frequency =>
      getPairs(antennas, frequency).foreach { pair =>
        val direction = pair._2.pos - pair._1.pos
        antinodes = antinodes + (pair._2.pos + direction)
        antinodes = antinodes + (pair._1.pos - direction)
      }
    }
    antinodes.filter(inRange(grid, _))

  private def getAntiNodes2(grid: Grid, antennas: Seq[Antenna]): Set[Vec2] =
    val types = antennas.map(_.frequency).distinct
    var antinodes = Set[Vec2]()
    types.foreach { frequency =>
      getPairs(antennas, frequency).foreach { pair =>
        val direction = pair._2.pos - pair._1.pos
        var antinode = pair._2.pos
        while inRange(grid, antinode) do
          antinodes = antinodes + antinode
          antinode = antinode + direction

        antinode = pair._1.pos
        while inRange(grid, antinode) do
          antinodes = antinodes + antinode
          antinode = antinode - direction
      }
    }
    antinodes.filter(inRange(grid, _))

  private def getPairs(antennas: Seq[Antenna], frequency: Char): Seq[(Antenna, Antenna)] =
    val sameType = antennas.filter(_.frequency == frequency)
    pairs(sameType)

  private def parse(input: Seq[String]) =
    val grid = makeGrid(input)
    val antennas = grid.indices.flatMap { y =>
      grid.head.indices.map { x =>
        if grid(y)(x) != '.' then
          Some(Antenna(Vec2(x, y), grid(y)(x)))
        else None
      }.collect { case Some(a) => a }
    }
    (grid, antennas)
