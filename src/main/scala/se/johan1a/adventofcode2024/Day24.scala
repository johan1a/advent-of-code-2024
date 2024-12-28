package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024.Utils.{split, splitOnce}

import scala.collection.mutable
import scala.util.Random

object Day24:

  sealed trait Op:
    def ref: String

  case class Literal(ref: String, value: Boolean) extends Op
  case class And(ref: String, a: String, b: String) extends Op
  case class Or(ref: String, a: String, b: String) extends Op
  case class Xor(ref: String, a: String, b: String) extends Op

  def part1(input: Seq[String]): Long =
    val ops = mutable.Map[String, Op]()
    parse(input).foreach(op => ops.put(op.ref, op))
    val zRefs = ops.filter(_._1.startsWith("z")).keys.toSeq.sorted.reverse
    val bits = zRefs.map(ref => compute(ops, ref, ref)).map(b => if b then "1" else "0").mkString
    java.lang.Long.parseLong(bits, 2)

  def part2(input: Seq[String], n: Int = 4): String =
    val opSeq = parse(input)
    val refs: Seq[String] = opSeq.map(_.ref)
    val ops: mutable.Map[String, Op] = mutable.Map[String, Op]() // opSeq.map(op => op.ref -> op).toMap
    opSeq.foreach { op =>
      ops.put(op.ref, op)
    }
    val nbrZ = refs.count(_.startsWith("z"))

    val allOnes: Long = java.lang.Long.parseLong(0.until(nbrZ).map(_ => "1").mkString, 2)
    val badOutputs = getBadOutputs(ops, allOnes, 0L)

    val possible = getPossibleToSwap(refs, badOutputs, usedBy)
    val combinations = getCombinations(possible)
    val combinationsWithDifferentOutput = combinations.filter { (a, b) =>
      results.get(a) != results.get(b)
    }

    var best = badOutputs

    var i = 0
    val cdo = combinationsWithDifferentOutput
    while i < cdo.size do
      println(s"i: $i")
      var j = i + 1
      while j < cdo.size && cdo(j)._1 == cdo(i)._1 do
        j += 1
      while j < cdo.size do
        println(s"j: $j / ${cdo.size}")
        var k = j + 1
        while k < cdo.size && cdo(k)._1 == cdo(j)._1 do
          k += 1
        while k < cdo.size do
          println(s"i: $i, j: $j, k: $k / ${cdo.size}")
          var l = k + 1

          while l < cdo.size && cdo(l)._1 == cdo(k)._1 do
            l += 1

          while l < cdo.size do
            val allUsed = (((Seq(cdo(i)) :+ cdo(j)) :+ cdo(k)) :+ cdo(l)).flatMap(pair => Seq(pair._1, pair._2))

            if allUsed.distinct.size == 8 then
              swap(ops, cdo(i)._1, cdo(i)._2)
              swap(ops, cdo(j)._1, cdo(j)._2)
              swap(ops, cdo(k)._1, cdo(k)._2)
              swap(ops, cdo(l)._1, cdo(l)._2)
              val badOutputs = getBadOutputs(ops, allOnes, 0L, analyze = false)
              if badOutputs.size < best.size then
                println(s"new best: ${badOutputs.size}")
                best = badOutputs
              if badOutputs.isEmpty then
                return allUsed.sorted.mkString(",")
              swap(ops, cdo(i)._2, cdo(i)._1)
              swap(ops, cdo(j)._2, cdo(j)._1)
              swap(ops, cdo(k)._2, cdo(k)._1)
              swap(ops, cdo(l)._2, cdo(l)._1)
            l += 1
          k += 1
        j += 1
      i += 1

    ""

  private def swap(ops: mutable.Map[String, Op], a: String, b: String) =
    val temp = ops(a)
    ops.put(a, ops(b))
    ops.put(b, temp)

  def getCombinations[T](seq: Seq[T]): Seq[(T, T)] =
    var combinations = Seq[(T, T)]()
    seq.indices.foreach { i =>
      (i + 1).until(seq.size).foreach { j =>
        combinations = combinations :+ (seq(i), seq(j))
      }
    }
    combinations

  private def getPossibleToSwap(refs: Seq[String], badOutputs: Seq[String], used: Map[String, Set[String]]) =
    val goodOutputs = refs.filter(r => r.startsWith("z") && !badOutputs.contains(r))
    refs.filter { ref =>
      val gatesUsed = used.getOrElse(ref, Set.empty)
      goodOutputs.forall(goodOutput =>
        !gatesUsed.contains(goodOutput)
      )
    }

  private def getBadOutputs(ops: mutable.Map[String, Op], x: Long, y: Long, analyze: Boolean = true): Seq[String] =
    put(ops, x, "x")
    put(ops, y, "y")
    val expectedBits = getPrefixedBits(x + y, "z")
    val expected = parseBits(expectedBits)
    assert(expected == x + y)

    val bits = computeToBits(ops, analyze)
    val badOutputs = bits.zip(expectedBits).filter { case (actual: (String, String), expected: (String, String)) =>
      actual._2 != expected._2
    }.map(_._1._1)
    badOutputs

  private def parseBits(bits: Seq[(String, String)]) =
    java.lang.Long.parseLong(bits.map(_._2).reverse.mkString, 2)

  private def put(ops: mutable.Map[String, Op], k: Long, prefix: String): Unit =
    val refs = ops.keys.toSeq.filter(_.startsWith(prefix))
    refs.foreach(ref =>
      ops.put(ref, Literal(ref, false))
    )
    val kBits = getPrefixedBits(k, prefix)
    kBits.foreach { (ref, value) =>
      ops.put(ref, Literal(ref, value == "1"))
    }

  def getPrefixedBits(k: Long, prefix: String): Seq[(String, String)] =
    k.toBinaryString.reverse.zipWithIndex.map { case (char, i) => getRef(i, prefix) -> char.toString }

  private def parseLiterals(ops: Seq[Op], prefix: String): Long =
    java.lang.Long.parseLong(
      ops.sortBy(_.ref).reverse.filter(_.ref.startsWith(prefix)).collect { case Literal(ref, value) =>
        if value then "1" else "0"
      }.mkString,
      2
    )

  var usedBy: Map[String, Set[String]] = Map()
  var results: Map[String, String] = Map()

  private def computeToBits(ops: mutable.Map[String, Op], analyze: Boolean = true) =
    usedBy = Map()
    results = Map()
    val zRefs = ops.keys.filter(_.startsWith("z")).toSeq.sorted.reverse
    zRefs.map(ref => (ref, if compute(ops, ref, ref, analyze) then "1" else "0")).reverse

  // 001001
  private def getRef(i: Int, prefix: String) =
    if i <= 9 then
      s"${prefix}0$i"
    else
      s"$prefix$i"

  private def compute(refs: mutable.Map[String, Op], ref: String, topRef: String, analyze: Boolean = true): Boolean =
    if analyze then
      val used: Set[String] = usedBy.getOrElse(ref, Set.empty)
      usedBy = usedBy + (ref -> (used + topRef))

    val result = refs(ref) match
      case Literal(ref, value) => value
      case And(ref, a, b)      => compute(refs, a, topRef) && compute(refs, b, topRef)
      case Or(ref, a, b)       => compute(refs, a, topRef) || compute(refs, b, topRef)
      case Xor(ref, a, b)      => compute(refs, a, topRef) ^ compute(refs, b, topRef)

    if analyze then
      results = results + (ref -> (if result then "1" else "0"))

    result

  private def parse(input: Seq[String]): Seq[Op] =
    val splitted = split(input)
    val literals = splitted.head.map(parseLiteral)
    val gates = splitted.last.map(parseGate)
    gates ++ literals

  private def parseLiteral(str: String): Literal =
    val (ref, value) = splitOnce(str, ": ")
    Literal(ref, value == "1")

  private def parseGate(str: String): Op = str match
    case s"$a AND $b -> $ref" => And(ref, a, b)
    case s"$a OR $b -> $ref"  => Or(ref, a, b)
    case s"$a XOR $b -> $ref" => Xor(ref, a, b)
