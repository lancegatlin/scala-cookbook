package org.ldg

import scala.collection.Factory

/**
  * Scala collection extension methods
  */
object CollectionExt {
  implicit class CatDigitalIterableExt[A, CC[AA] <: Iterable[AA]]( val self: CC[A] ) extends AnyVal {

    /**
      * Group elements in a collection into subgroups of sequential elements (i.e. runs) using a custom weight
      * calculation where the total weight of a run/group with multiple elements is never more than some maximum weight.
      * If a single element exceeds the maximum weight then that element is emitted in a run/group containing only that
      * element.
      *
      * @param maxWeightInclusive maximum total weight to allow in a group of multiple elements
      * @param f function to compute the weight of an element
      * @param factory factory for building new collection
      * @return an iterator for a groups of elements where the total weight of each group containing multiple elements is
      *         never higher than maxWeightInclusive
      */
    def groupWeightedMaxBy( maxWeightInclusive: Long )( f: A => Long )( implicit factory: Factory[A, CC[A]] ): Iterator[CC[A]] = {
      val bufferedIterator = self.iterator.buffered

      new Iterator[CC[A]] {
        override def hasNext: Boolean = bufferedIterator.hasNext
        override def next(): CC[A] = {
          val builder = factory.newBuilder
          var weightSoFar = f( bufferedIterator.head )
          builder.addOne( bufferedIterator.next() )
          var done = weightSoFar > maxWeightInclusive

          while (done == false && bufferedIterator.hasNext) {
            val newWeight = weightSoFar + f( bufferedIterator.head )
            if (newWeight <= maxWeightInclusive) {
              builder.addOne( bufferedIterator.next() )
              weightSoFar = newWeight
            } else {
              done = true
            }
          }

          builder.result()
        }
      }
    }
  }

}
