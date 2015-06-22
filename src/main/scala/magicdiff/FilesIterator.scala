package magicdiff

import java.io.File
import scala.collection.JavaConversions._

case class FilesIterator(rootFolder: String, keepFiles: File => Boolean = _ => true) {
  val prefix = rootFolder.length
  
  def filesRec(root: File = new File(rootFolder)): Map[String, File] = {
    val (files, folders) = root.listFiles().toList.partition(_.isFile)
    files.filter(keepFiles).map(f => f.getPath.substring(prefix) -> f).toMap ++ folders.flatMap(filesRec)
  }
}

object FilesIteratorDemo extends App {
  println(FilesIterator("a", _.getName.endsWith(".java")).filesRec())
}