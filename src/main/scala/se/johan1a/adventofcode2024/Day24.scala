package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024.Utils.{split, splitOnce}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths, StandardOpenOption}
import scala.collection.mutable
import scala.util.Try

object Day24:

  sealed trait Op:
    def ref: String
    def opType: String

  case class Literal(ref: String, value: Boolean) extends Op:
    def opType = "literal"
  case class And(ref: String, a: String, b: String) extends Op:
    def opType = "And"
  case class Or(ref: String, a: String, b: String) extends Op:
    def opType = "Or"
  case class Xor(ref: String, a: String, b: String) extends Op:
    def opType = "Xor"

  def part1(input: Seq[String]): Long =
    val ops = mutable.Map[String, Op]()
    parse(input).foreach(op => ops.put(op.ref, op))
    val zRefs = ops.filter(_._1.startsWith("z")).keys.toSeq.sorted.reverse
    val bits = zRefs.map(ref => compute(ops, ref, ref)).map(b => if b then "1" else "0").mkString
    java.lang.Long.parseLong(bits, 2)

  def part2(input: Seq[String], n: Int = 4): String =
    val opSeq = parse(input)
    val refs: Seq[String] = opSeq.map(_.ref).toArray
    val ops: mutable.Map[String, Op] = mutable.Map[String, Op]() // opSeq.map(op => op.ref -> op).toMap
    opSeq.foreach { op =>
      ops.put(op.ref, op)
    }
    val nbrZ = refs.count(_.startsWith("z"))

    val a = Math.pow(2, 10).toLong
    val b = 1L
    val badOutputs = getBadOutputs(ops, a, b)

    // Manually look at stdout and swap accordingly
    swap(ops, "wjb", "cvp")
    swap(ops, "wcb", "z34")
    swap(ops, "mkk", "z10")
    swap(ops, "qbw", "z14")

    validate(ops)
    val pairs: Seq[(String, String)] = getCombinations(candidates)
    println(s"Got ${candidates.size} candidates, ${pairs.size} pairs")

    Seq("wjb", "cvp", "wcb", "z34", "mkk", "z10", "qbw", "z14").sorted.mkString(",")

  var candidates: Seq[String] = Seq()

  private def validate(ops: mutable.Map[String, Day24.Op]) =
    val zRefs = ops.keys.filter(_.startsWith("z")).toSeq.sorted
    // Skip edges, I think they are structured differently
    2.until(zRefs.size - 1).reverse.foreach { i =>
      val zRef = getRef(i, "z")
      val result = ops(zRef) match
        case Xor(ref, a, b) =>
          validateZi(ops, ops(a), ops(b), i, ref) && existsAnd(ops, getRef(i, "x"), getRef(i, "y"), i)
        case other =>
          candidates = candidates :+ zRef
          println(s"Wanted Xor but got $other at $i for: $zRef")

//      xi xor yi -> a_i
//      xi and yi -> b_i
//      ai and carry_(i - 1) -> c_i
//      ai xor carry_(i - 1) -> z_i
//      b_i or c_i -> carry_i

    }

  private def existsAnd(ops: mutable.Map[String, Op], a: String, b: String, i: Int): Boolean =
    val result = ops.collect {
      case (_, And(ref, left, right)) if left == a && right == b || left == b && right == a => true
    }
      .size == 1
    if !result then
      println(s"Wanted And(${a}, ${b}) but none found at $i")

    result

  private def validateZi(ops: mutable.Map[String, Op], left: Op, right: Op, i: Int, ref: String): Boolean =
    (left, right) match
      case (Xor(_, a, b), Or(prevCarry, c, d)) =>
        validateAi(ops, ops(a), ops(b), i) && validatePrevCarry(ops, ops(c), ops(d), i, prevCarry)
      case (Or(prevCarry, c, d), Xor(_, a, b)) =>
        validateAi(ops, ops(a), ops(b), i) && validatePrevCarry(ops, ops(c), ops(d), i, prevCarry)
      case other =>
        println(s"Wanted (Xor,Or) but got $other at $i for: $ref")
        candidates = candidates :+ ref
        false

  private def validateAi(ops: mutable.Map[String, Op], left: Op, right: Op, i: Int): Boolean =
    (left, right) match
      case (Literal(xRef, _), Literal(yRef, _)) if xRef == getRef(i, "x") && yRef == getRef(i, "y") => true
      case (Literal(yRef, _), Literal(xRef, _)) if xRef == getRef(i, "x") && yRef == getRef(i, "y") => true
      case other =>
        println(s"Wanted (Literal,Literal) but got $other at $i")
        false

  private def validatePrevCarry(
      ops: mutable.Map[String, Op],
      left: Op,
      right: Op,
      i: Int,
      prevCarryRef: String
  ): Boolean =
    (left, right) match
      case (And(_, _, _), And(_, _, _)) => true
      case other =>
        println(s"Wanted (And,And) but got $other for: $prevCarryRef")
        candidates = candidates :+ prevCarryRef
        false

  private def writeGraphViz(ops: mutable.Map[String, Op]) =
    val path = Paths.get("output.graph")

    val sb = new StringBuilder()

    sb.append("digraph TheGraph {\n")

    ops.foreach { case (ref, op) =>
      val str = op match
        case Literal(ref, value) =>
          ""
        case And(ref, a, b) =>
          sb.append(s"$a -> $ref [label=\"and\"];\n")
          sb.append(s"$b -> $ref [label=\"and\"];\n")
        case Or(ref, a, b) =>
          sb.append(s"$a -> $ref [label=\"or\"];\n")
          sb.append(s"$b -> $ref [label=\"or\"];\n")
        case Xor(ref, a, b) =>
          sb.append(s"$a -> $ref [label=\"xor\"];\n")
          sb.append(s"$b -> $ref [label=\"xor\"];\n")
    }
    sb.append("\n}")

    Try {
      Files.delete(path)
    }
    Files.write(
      path,
      sb.toString.getBytes(StandardCharsets.UTF_8),
      StandardOpenOption.CREATE,
      StandardOpenOption.WRITE
    )

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
    try

      val expectedBits = getPrefixedBits(x + y, "z")
      val expected = parseBits(expectedBits)
      assert(expected == x + y)

      val bits = doCompute(ops, x, y, analyze)
      val badOutputs = bits.zip(expectedBits).filter { case (actual: (String, String), expected: (String, String)) =>
        actual._2 != expected._2
      }.map(_._1._1).toArray
      badOutputs
    catch
      case e: Exception => ops.keys.filter(_.startsWith("z")).toSeq

  private def doCompute(
      ops: mutable.Map[String, Op],
      x: Long,
      y: Long,
      analyze: Boolean = true
  ): Seq[(String, String)] =
    usedBy = Map()
    results = Map()
    put(ops, x, "x")
    put(ops, y, "y")
    val bits = computeToBits(ops, analyze)
    bits

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

  private def getRef(i: Int, prefix: String) =
    if i <= 9 then
      s"${prefix}0$i"
    else
      s"$prefix$i"

  val maxDepth = 1000

  private def compute(
      refs: mutable.Map[String, Op],
      ref: String,
      topRef: String,
      analyze: Boolean = true,
      depth: Int = 0
  ): Boolean =
    if depth > maxDepth then
      throw Exception("max depth reached")
    if analyze then
      val used: Set[String] = usedBy.getOrElse(ref, Set.empty)
      usedBy = usedBy + (ref -> (used + topRef))

    val op = refs(ref)
    val result = op match
      case Literal(ref, value) => value
      case And(ref, a, b) => compute(refs, a, topRef, depth = depth + 1) && compute(refs, b, topRef, depth = depth + 1)
      case Or(ref, a, b)  => compute(refs, a, topRef, depth = depth + 1) || compute(refs, b, topRef, depth = depth + 1)
      case Xor(ref, a, b) => compute(refs, a, topRef, depth = depth + 1) ^ compute(refs, b, topRef, depth = depth + 1)

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
