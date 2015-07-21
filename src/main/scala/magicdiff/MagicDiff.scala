package magicdiff

import java.io.File

object MagicDiff extends App {
  val comparator = DiffComparator

  val left = if (args.length > 0) args(0) else "a"
  val right = if (args.length > 1) args(1) else "b"

  val leftFiles = FilesIterator(left, keepJava).filesRec()
  val rightFiles = FilesIterator(right, keepJava).filesRec()
  val all = (leftFiles ++ rightFiles).toList.distinct

  def status(f: String) = {
    val (isLeft, isRight) = (leftFiles.contains(f), rightFiles.contains(f))
    if (isLeft && !isRight) Left
    else if (!isLeft && isRight) Right
    else comparator.compare(leftFiles(f), rightFiles(f))
  }

  def keepJava(f: File) = f.getName.endsWith("java")

  val results = (for {
    (name, f) <- all
    st = status(name)
  } yield (name, st)).groupBy(_._2).toList.sortBy(_._1)(ComparisonOrder)

  for {
    (status, elts) <- results
  } {
    for {
      (name, _) <- elts.sortBy(_._1)
    } println(s"$status\t$name")
  }

}