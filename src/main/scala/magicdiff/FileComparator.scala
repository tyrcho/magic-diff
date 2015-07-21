package magicdiff

import java.io.File
import scala.io.Source
import difflib.DiffUtils
import scala.collection.JavaConversions._
import difflib.Delta
import scala.annotation.tailrec

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

  def trimLines(lines: List[String]) = {
    val uncomment = removeCommentsAndSpaces(lines)
    val noImports = removeImports(uncomment)
    val brackets = fixBrackets(noImports).map(_.trim)
    val noBlanks = removeBlanks(brackets)
    noBlanks
  }

  def removeBlanks(lines: List[String]) =
    lines.filterNot(_.isEmpty)

  def removeImports(lines: List[String]) =
    lines.filterNot(_ matches """\s*import.*""")

    
  @tailrec
  def fixBrackets(lines: List[String], processed: List[String] = Nil, last: String = ""): List[String] = lines match {
    case Nil => processed
    case h :: t =>
      if (h.startsWith("}") && h.length > 1) {
        val text = h.drop(1)
        fixBrackets("}" :: t, processed ++ List(last), text)
      } else if (h == "{") fixBrackets(t, processed, last + " {")
      else fixBrackets(t, processed ++ List(last), h)
  }

  def removeCommentsAndSpaces(lines: List[String]) = {
    val file = lines.map(_.trim).mkString("\n")
    val noMulti = file.replaceAll("""(?s)\/\*.*?\*\/""", "")
    val singleSpace = noMulti.replaceAll("""[ \t]+""", " ")
    val noSingleComment = singleSpace.replaceAll("""\/\/.*""", " ")
    val ignoreLogMessage = noSingleComment.replaceAll("""(?sm)format\(.*?;$""", """format("...");""")
    ignoreLogMessage.split("\n").toList.map(_.trim)
  }

  def compare(a: File, b: File): Comparison = {
    val (linesA, linesB) = (readFile(a), readFile(b))
    val deltas = DiffUtils.diff(linesA, linesB).getDeltas
    if (deltas.isEmpty) Equals
    else {
      val (trimA, trimB) = (trimLines(linesA), trimLines(linesB))
      val diffs = DiffUtils.diff(trimA, trimB).getDeltas
      if (diffs.isEmpty) Similar
      else Different(diffs.toList)
    }
  }
}