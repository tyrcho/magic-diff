package magicdiff

import java.io.File
import scala.io.Source
import difflib.DiffUtils
import scala.collection.JavaConversions._
import difflib.Delta

trait FileComparator {
  def compare(a: File, b: File): Comparison
}

object ContentComparator extends FileComparator {
  def readFile(f: File) = Source.fromFile(f).getLines.toList

  def compare(a: File, b: File): Comparison = {
    if (readFile(a) == readFile(b)) Equals
    else Different(Nil)
  }
}

object DiffComparator extends FileComparator {
  def readFile(f: File) = Source.fromFile(f).getLines.toList

  def trimLines(lines: List[String]) = lines.filter(l => ignorePatterns.forall(p => !l.matches(p)))

  def compare(a: File, b: File): Comparison = {
    val (linesA, linesB) = (readFile(a), readFile(b))
    val deltas = DiffUtils.diff(linesA, linesB).getDeltas
    if (deltas.isEmpty) Equals
    else {
      val diffs = DiffUtils.diff(trimLines(linesA), trimLines(linesB)).getDeltas
      if (diffs.isEmpty) Similar
      else Different(diffs.toList)
    }
  }

  // ignore imports, comments
  val ignorePatterns = Seq(
    """\s*import.*""",
    """\s*\*.*""",
    """\s*\/\*.*""",
    """\s*\/\/.*""")

  //  def keep(d: Delta): Boolean = {
  //    val lines = (d.getOriginal.getLines ++ d.getRevised.getLines).map(_.toString.trim)
  //    lines.exists(l => l.nonEmpty
  //      && ignorePatterns.forall(p => !l.matches(p)))
  //  }
}