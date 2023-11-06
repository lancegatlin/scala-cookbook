package org.ldg.test

import org.scalacheck.Gen

object FiniteGenEx {
  /*
    By default all ScalaCheck Gen are expected to generate infinite values. Gen sample failures are expected to be
    retry-able. FiniteGenEx extends ScalaCheck Gen to allow creating Gen that are finite. They produce a certain number
    of values and then fail permanently and this failure will never succeed if retried.

    The purpose of a finite Gen is to allow prefixing specific values to a standard infinite Gen which might not always
    generate those values in a particular test run to ensure they are always generated first during the test run. Since
    forAll expects Gen to be infinite, finite Gen should always use Gen.andThen on a standard infinite genrator.

    Example:

      val stringGen1 = Gen.asciiStr
      val stringGen2 =
        Gen.once("") andThen
        Gen.once(Gen.asciiChar.map(_.toString)) andThen
        Gen.asciiStr

    stringGen1 may not always generate empty string and may not always generate exactly one character in a particular
    test run. stringGen2 guarantees that every test run will start with empty string and then a string with exactly
    one character and after that uses the standard infinite Gen.
   */

  implicit class GenExt[A](val self: Gen[A]) extends AnyVal {

    /**
     * @return a finite Gen that delegates to self until first failure and then delegates to other.
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
     * @return an infinite Gen that alternates between delegating to self and other
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
     * @return a finite Gen that generates one value from delegate Gen
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
     * @return a finite Gen that generates all values in xa and then fails
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
     * @return a finite Gen that generates all iterables in gxa and all values in each iterable and then fails
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
