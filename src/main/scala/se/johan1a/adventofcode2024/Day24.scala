package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024.Utils.{split, splitOnce}
import scala.collection.mutable

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
    val bits = zRefs.map(ref => compute(ops, ref)).map(b => if b then "1" else "0").mkString
    java.lang.Long.parseLong(bits, 2)

  def part2(input: Seq[String], n: Int = 4): String =
    val opSeq = parse(input)
    val refs = opSeq.map(_.ref)
    val ops: mutable.Map[String, Op] = mutable.Map[String, Op]() // opSeq.map(op => op.ref -> op).toMap
    opSeq.foreach { op =>
      ops.put(op.ref, op)
    }
    val zRefs = refs.filter(_.startsWith("z")).sorted.reverse
    val x = parseLiterals(opSeq, "x")
    val y = parseLiterals(opSeq, "y")
    findFlipped(ops, zRefs, x, y, n).flatMap(tuple => Seq(tuple._1, tuple._2)).sorted.mkString(",")

  private def put(ops: mutable.Map[String, Op], k: Long, prefix: String): Unit =
    val refs = ops.keys.toSeq.filter(_.startsWith(prefix))
    refs.foreach(ref =>
      ops.remove(ref)
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

  private def findFlipped(
      ops: mutable.Map[String, Op],
      zRefs: Seq[String],
      x: Long,
      y: Long,
      n: Int
  ): Seq[(String, String)] =
    val expectedBits = getPrefixedBits(x + y, "z")
    val bits = computeToBits(ops.toMap, zRefs)
    val sum = java.lang.Long.parseLong(bits, 2)
    sum == x + y
    Seq.empty

  private def computeToBits(ops: Map[String, Op], zRefs: Seq[String]) =
    zRefs.map(ref => compute(ops, ref)).map(b => if b then "1" else "0").mkString

  // 001001
  private def getRef(i: Int, prefix: String) =
    if i <= 9 then
      s"${prefix}0$i"
    else
      s"$prefix$i"

  private def compute(refs: Map[String, Op], ref: String): Boolean =
    refs(ref) match
      case Literal(ref, value) => value
      case And(ref, a, b)      => compute(refs, a) && compute(refs, b)
      case Or(ref, a, b)       => compute(refs, a) || compute(refs, b)
      case Xor(ref, a, b)      => compute(refs, a) ^ compute(refs, b)

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
