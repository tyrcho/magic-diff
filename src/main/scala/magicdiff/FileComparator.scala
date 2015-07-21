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

  def compare(a: File, b: File): Comparison = {
    val deltas = DiffUtils.diff(readFile(a), readFile(b)).getDeltas
    if (deltas.isEmpty) Equals
    else {
      val diffs = deltas.filter(keep)
      if (diffs.isEmpty) Similar
      else Different(diffs.toList)
    }
  }

  def keep(d: Delta): Boolean = {
    val lines = (d.getOriginal.getLines ++ d.getRevised.getLines).map(_.toString.trim)
    lines.exists(l => l.nonEmpty && !l.startsWith("import"))
  }
}