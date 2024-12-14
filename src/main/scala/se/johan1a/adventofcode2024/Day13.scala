package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024.Utils.*

import java.math.{MathContext, RoundingMode}
import java.net.URLEncoder

object Day13:

  case class Machine(a: Vec2, b: Vec2, prize: Pos)

  var cache: Map[(Vec2, Int), Option[Int]] = Map()

  def part1(input: Seq[String]): Int =
    parse(input).map(machine =>
      cache = Map()
      best(machine)
    ).map(_.getOrElse(0)).sum

  val precision = BigDecimal("0.00000000000000000000000000001")
  val mc = MathContext(128, RoundingMode.HALF_EVEN)

  def part2(input: Seq[String], k: Long = 10000000000000L): BigInt =
    // Button A: X+94, Y+34
    // Button B: X+22, Y+67
    // Prize: X=10000000008400, Y=10000000005400

    // 10000000008400 = a * 94 + b * 22
    // 10000000005400 = a * 34 + b * 67

    // 10000000008400 - b * 22 = a * 94
    // (10000000008400 - b * 22)/94 = a

    // 10000000005400 = a * 34 + b * 67
    // 10000000005400 = (10000000008400 - b * 22)/94 * 34 + b * 67
    // 10000000005400 = 10000000008400/94*34 - b * 22/94 * 34 + b * 67
    // 10000000005400 = 10000000008400/94*34 + b * 67 - b * 22/94 * 34
    // 10000000005400 = 10000000008400/94*34 + b * (67 - 22/94 * 34)
    // 10000000005400 - 10000000008400/94*34 = b * (67 - 22/94 * 34)
    // (10000000005400 - 10000000008400/94*34) / (67 - 22/94 * 34) = b
    // b = (10000000005400 - 10000000008400 / 94*34) / (67 - 22 / 94 * 34)
    // b = (prizeY - prizeX / aX * aY) / (bY - bX / aX * aY)

    // prizeX = a * aX + b * bX
    // prizeX - b * bx = a * aX
    // (prizeX - b * bx) / aX = a
    // a = (prizeX - b * bx) / aX
    parse(input).map { machine =>
      val prize = machine.prize
      val a = machine.a
      val b = machine.b
      val prizeX = BigDecimal(prize.x + k, mc)
      val prizeY = BigDecimal(prize.y + k, mc)
      val aX = BigDecimal(a.x, mc)
      val aY = BigDecimal(a.y, mc)
      val bX = BigDecimal(b.x, mc)
      val bY = BigDecimal(b.y, mc)

      // b = (prizeY - prizeX / aX * aY) / (bY - bX / aX * aY)
      // a = (prizeX - b * bx) / aX
      val k0 = prizeX / aX

      val k1 = bX / aX
      val k2 = k0 * aY
      val k3 = k1 * aY
      val k4 = prizeY - k2
      val k5 = bY - k3
      val nbrB = k4 / k5
      val k6 = nbrB * bX
      val k7 = prizeX - k6
      val nbrA = k7 / aX

      val bs = s"($prizeY-$prizeX/$aX*$aY)/($bY-$bX/$aX*$aY)"
      val bQuery = URLEncoder.encode(bs, "UTF-8")

      val as = s"($prizeX-${nbrB.toBigInt}*$bX)/$aX"
      val aQuery = URLEncoder.encode(as, "UTF-8")
      println(s"\na $nbrA b $nbrB")
//      println(s"a: https://duckduckgo.com/?q=$aQuery")
//      println(s"b: https://duckduckgo.com/?q=$bQuery")
      println(s"a: https://www.wolframalpha.com/input?i=$aQuery")
      println(s"b: https://www.wolframalpha.com/input?i=$bQuery")

//      val large = 10000000000000L
//      val ppX = BigDecimal(prize.x)
//      val ppY = BigDecimal(prize.y)
//      val b2 = (large/10000L) / ((bY - bX / aX * aY)/10000L) + (ppY - (large/100000L) / (aX/100000) * aY - ppX / aX * aY) / (bY - bX / aX * aY)
//      val a2 = large / aX + ppX / aX - b2 * bX / aX

      if Seq(nbrA, nbrB).exists(d => !isInteger(d)) then
        BigInt(0)
      else
        println(s"a: $nbrA b: $nbrB")
        3 * round(nbrA) + round(nbrB)
    }.sum

  private def round(d: BigDecimal): BigInt =
    val remainder = d.remainder(1).abs
    val result = if remainder < 0.5 then
      d.toBigInt
    else
      d.toBigInt + 1
    result

  private def isInteger(nbrA: BigDecimal) =
    val remainder = nbrA.remainder(1).abs
    val result = if remainder < 0.5 then
      remainder <= precision
    else
      (1 - remainder) <= precision
    result

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

//
// b = (prizeY - prizeX / aX * aY) / (bY - bX / aX * aY)
