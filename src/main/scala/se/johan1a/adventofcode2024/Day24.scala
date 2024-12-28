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
    val ops = parse(input).map(op => op.ref -> op).toMap
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

    val possible = getPossibleToSwap(refs, badOutputs, usedBy.toMap)

    ""

  private def getPossibleToSwap(refs: Seq[String], badOutputs: Seq[String], used: Map[String, Set[String]]) =
    val goodOutputs = refs.filter(r => r.startsWith("z") && !badOutputs.contains(r))
    refs.filter { ref =>
      val gatesUsed = used.getOrElse(ref, Set.empty)
      goodOutputs.forall(goodOutput =>
        !gatesUsed.contains(goodOutput)
      )
    }

  private def getBadOutputs(ops: mutable.Map[String, Op], x: Long, y: Long): Seq[String] =
    put(ops, x, "x")
    put(ops, y, "y")
    val expectedBits = getPrefixedBits(x + y, "z")
    val expected = parseBits(expectedBits)
    assert(expected == x + y)

    val bits = computeToBits(ops.toMap)
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

  private def computeToBits(ops: Map[String, Op]) =
    usedBy = Map()
    val zRefs = ops.keys.filter(_.startsWith("z")).toSeq.sorted.reverse
    zRefs.map(ref => (ref, if compute(ops, ref, ref) then "1" else "0")).reverse

  // 001001
  private def getRef(i: Int, prefix: String) =
    if i <= 9 then
      s"${prefix}0$i"
    else
      s"$prefix$i"

  private def compute(refs: Map[String, Op], ref: String, topRef: String): Boolean =
    val used: Set[String] = usedBy.getOrElse(ref, Set.empty)
    usedBy = usedBy + (ref -> (used + topRef))
    refs(ref) match
      case Literal(ref, value) => value
      case And(ref, a, b)      => compute(refs, a, topRef) && compute(refs, b, topRef)
      case Or(ref, a, b)       => compute(refs, a, topRef) || compute(refs, b, topRef)
      case Xor(ref, a, b)      => compute(refs, a, topRef) ^ compute(refs, b, topRef)

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
