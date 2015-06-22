package magicdiff

import java.io.File
import scala.io.Source

trait FileComparator {
  def compare(a: File, b: File): String
}

object ContentComparator extends FileComparator {
  def readFile(f: File) = Source.fromFile(f).getLines.toList

  def compare(a: File, b: File): String = {
    if (readFile(a) == readFile(b)) "equals" else "diff"
  }
}