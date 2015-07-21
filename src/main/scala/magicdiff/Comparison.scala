package magicdiff

import difflib.Delta

trait Comparison { def ord: Int }

case object Left extends Comparison { val ord = -4 }
case object Right extends Comparison { val ord = -3 }
case object Similar extends Comparison { val ord = -2 }
case object Equals extends Comparison { val ord = -1 }
case class Different(deltas: List[Delta]) extends Comparison {
  val ord = deltas.size

  override def toString = s"${deltas.size} diffs : $deltas"
}
object ComparisonOrder extends Ordering[Comparison] {
  def compare(a: Comparison, b: Comparison) = b.ord - a.ord
}