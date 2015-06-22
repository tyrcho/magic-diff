package magicdiff

import java.io.File

object MagicDiff extends App {
  val comparator = DiffComparator

  val left = if (args.length > 0) args(0) else "a"
  val right = if (args.length > 1) args(1) else "b"

  val leftFiles = FilesIterator(left).filesRec()
  val rightFiles = FilesIterator(right).filesRec()
  val all = (leftFiles ++ rightFiles).toSet

  def status(f: String) = {
    if (leftFiles.contains(f) && !rightFiles.contains(f)) "left"
    else if (!leftFiles.contains(f) && rightFiles.contains(f)) "right"
    else comparator.compare(leftFiles(f), rightFiles(f))
  }

  for {
    (name, f) <- all
    if name.endsWith("java")
  } {
    println(s"$name,${status(name)}")
  }
}