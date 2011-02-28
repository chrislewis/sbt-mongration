/*
 You want https://github.com/scalaj/scalaj-collection
 
 This plugin targets sbt 0.7, which requires scala 2.7,
 and scalaj needs 2.8.
 */

package mongo.compat.collections.sj

import java.util.AbstractList

/* Seq => ju List wrapper. */
private [sj] class SeqWrapper[A](val underlying: Seq[A]) extends AbstractList[A] {
  override def size(): Int = underlying.size
  override def get(index: Int): A = underlying(index)
}
/* Implicit surrogate. */
private [sj] class RichSeq[A](val seq: Seq[A]) {
  def asJava = new SeqWrapper(seq)
}

object Implicits {
  implicit def seq2JList[A](seq: Seq[A]) = new RichSeq(seq)
}
