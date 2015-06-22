package magicdiff

import java.io.File
import scala.io.Source
import difflib.DiffUtils
import scala.collection.JavaConversions._
import difflib.Delta

trait FileComparator {
  def compare(a: File, b: File): String
}

object ContentComparator extends FileComparator {
  def readFile(f: File) = Source.fromFile(f).getLines.toList

  def compare(a: File, b: File): String = {
    if (readFile(a) == readFile(b)) "equals" else "diff"
  }
}

object DiffComparator extends FileComparator {
  def readFile(f: File) = Source.fromFile(f).getLines.toList

  def compare(a: File, b: File): String = {
    val deltas = DiffUtils.diff(readFile(a), readFile(b)).getDeltas
    if (deltas.isEmpty)
      "equals"
    else explain(deltas.filter(!ignore(_)).toList)
  }

  def ignore(d: Delta): Boolean = {
    val lines = (d.getOriginal.getLines ++ d.getRevised.getLines).map(_.toString.trim)
    lines.forall(l => l.isEmpty || l.startsWith("import"))
  }

  def explain(deltas: List[Delta]) = {
    val count = deltas.size
    s"$count deltas ($deltas)"
  }
}