package org.ldg.test

import org.scalacheck.Gen

object GenEx {
  implicit class GenExt[A](val self: Gen[A]) extends AnyVal {

    /**
     * @return a Gen that delegates to self until first failure then delegates to other.
     *         If self is infinite and never fails then other is never used
     */
    def andThen(other: Gen[A]): Gen[A] = {
      var flag = true

      Gen.delay {
        val retv =
          if (flag) {
            self.sample match {
              case None =>
                flag = false
                other.sample
              case s => s
            }
          } else {
            other.sample
          }
        retv.fold[Gen[A]](Gen.fail)(Gen.const)
      }
    }

    /**
     * @return a Gen that alternates between delegating to self and other
     */
    def zipConcat(other: Gen[A]): Gen[A] = {
      var flag = false

      Gen.delay {
        flag = !flag
        val retv =
          if (flag) {
            self.sample.orElse(other.sample)
          } else {
            other.sample.orElse(self.sample)
          }
        retv.fold[Gen[A]](Gen.fail)(Gen.const)
      }
    }
  }

  implicit class GenObjectExt(val self: Gen.type) extends AnyVal {

    /**
     * @return a Gen that generates one value and then fails
     */
    def once[A](a: A): Gen[A] = {
      var flag = true

      Gen.delay {
        if (flag) {
          flag = false
          Gen.const(a)
        } else {
          Gen.fail
        }
      }
    }

    /**
     * @return a Gen that generates one value from delegate Gen
     */
    def once[A](ga: Gen[A]): Gen[A] = {
      var flag = true

      Gen.delay {
        if (flag) {
          flag = false
          ga.sample.fold[Gen[A]](Gen.fail)(Gen.const)
        } else {
          Gen.fail
        }
      }
    }

        /**
     * @return a Gen that generates all values in xa and then fails
     */
    def allOf[A](xa: Iterable[A]): Gen[A] = {
      val iterator = xa.iterator

      Gen.delay {
        if (iterator.hasNext) {
          Gen.const(iterator.next())
        } else {
          Gen.fail
        }
      }
    }

    /**
     * @return a Gen that generates all iterables in gxa and all values in each iterable and then fails
     */
    def allOf[A](gxa: Gen[Iterable[A]]): Gen[A] = {
      var inner: Option[Gen[A]] = gxa.sample.map(allOf)

      def loop: Gen[A] = {
        inner match {
          case None =>
            Gen.fail
          case Some(innerGen) =>
            innerGen.sample match {
              case Some(a) =>
                Gen.const(a)
              case None =>
                inner = gxa.sample.map(allOf)
                loop
            }
        }
      }

      Gen.delay(loop)
    }

  }
}
