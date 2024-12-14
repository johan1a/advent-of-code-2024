package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024.Utils.*

object Day13:

  case class Machine(a: Vec2, b: Vec2, prize: Pos)

  var cache: Map[(Vec2, Int), Option[Int]] = Map()

  def part1(input: Seq[String]): Int =
    parse(input).map(machine =>
      cache = Map()
      best(machine)
    ).map(_.getOrElse(0)).sum

  def part2(input: Seq[String]): Int =
    -1

  private def best(
      machine: Machine,
      pos: Pos = Vec2(0, 0),
      cost: Int = 0,
      nbrAPresses: Int = 0,
      nbrBPresses: Int = 0
  ): Option[Int] =
    if cache.contains((pos, cost)) then
      cache((pos, cost))
    else
      val result = if nbrAPresses > 100 || nbrBPresses > 100 || pos.x > machine.prize.x || pos.y > machine.prize.y then
        None
      else if pos == machine.prize then
        Some(cost)
      else
        val pressA = best(machine, pos + machine.a, 3 + cost, nbrAPresses + 1, nbrBPresses)
        val pressB = best(machine, pos + machine.b, 1 + cost, nbrAPresses, nbrBPresses + 1)
        (pressA, pressB) match
          case (Some(a), Some(b)) => Some(Math.min(a, b))
          case (Some(a), None)    => Some(a)
          case (None, Some(b))    => Some(b)
          case _                  => None

      cache = cache + ((pos, cost) -> result)
      result

  private def parse(input: Seq[String]): Seq[Machine] =
    split(input).map { lines =>
      val a = numbers(lines(0))
      val aDiff = Vec2(a.head, a.last)
      val b = numbers(lines(1))
      val bDiff = Vec2(b.head, b.last)
      val prize = numbers(lines(2))
      val prizePos = Vec2(prize.head, prize.last)
      Machine(aDiff, bDiff, prizePos)
    }
