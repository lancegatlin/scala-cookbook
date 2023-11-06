package org.ldg.test

import org.scalacheck.Gen
import org.scalacheck.rng.Seed

/**
  By default all ScalaCheck Gen are expected to generate infinite values. Gen sample failures are expected to be
  retry-able. FiniteGen extends ScalaCheck Gen to allow creating Gen that are finite. They produce a certain number
  of values and then fail permanently and this failure will never succeed if retried.

  The purpose of a FiniteGen is to allow prefixing specific values to a standard infinite Gen which might not always
  generate those values in a particular test run to ensure they are always generated first during the test run. Since
  forAll expects Gen to be infinite, a FiniteGen must always use Gen.andThen on a standard infinite generator to
  convert from FiniteGen to Gen.

  Example:

    val stringGen1: Gen[String] = Gen.asciiStr
    val stringGen2: Gen[String] =
      FiniteGen.once("") andThen
      FiniteGen.once(Gen.asciiChar.map(_.toString)) andThen
      Gen.asciiStr

  stringGen1 may not always generate empty string and may not always generate exactly one character in a particular
  test run. stringGen2 guarantees that every test run will start with empty string and then a string with exactly
  one character and after that uses the standard infinite Gen.

 Note: due to ScalaCheck implementation choices, FiniteGen currently always uses a random seed. Though this isn't
  expected to be an issue since FiniteGen generated values are always generated every test run and thus are easily
  reproducible by prefixing FixitureGen.const of failing value.
 */
sealed trait FiniteGen[+A] {
  protected def asGen: Gen[A]

  def apply(p: Gen.Parameters, seed: Seed): Option[A] = asGen.apply(p, seed)
  def sample: Option[A] = asGen.sample

  /**
   * @return a finite Gen that delegates to self until first failure and then delegates to other.
   *         If self is infinite and never fails then other is never used
   */
  def andThen[AA >: A](other: Gen[AA]): Gen[A] = {
    var flag = true

    Gen.delay {
      val retv =
        if (flag) {
          asGen.sample match {
            case None =>
              flag = false
              other.sample
            case s => s
          }
        } else {
          other.sample
        }
      retv.fold[Gen[A]](Gen.fail)(aa => Gen.const(aa.asInstanceOf[A]))
    }
  }

  /**
   * @return a finite Gen that delegates to self until first failure and then delegates to other.
   *         If self is infinite and never fails then other is never used
   */
  def andThen[AA >: A](other: FiniteGen[AA]): FiniteGen[AA] =
    FiniteGen(andThen(other.asGen))

  /**
   * @return an infinite Gen that alternates between delegating to self and other
   */
  def zipConcat[AA >: A](other: Gen[AA]): Gen[A] = {
    var flag = false

    Gen.delay {
      flag = !flag
      val retv =
        if (flag) {
          asGen.sample.orElse(other.sample)
        } else {
          other.sample.orElse(asGen.sample)
        }
      retv.fold[Gen[A]](Gen.fail)(aa => Gen.const(aa.asInstanceOf[A]))
    }
  }

  def zipConcat[AA >: A](other: FiniteGen[AA]): FiniteGen[A] =
    FiniteGen(zipConcat(other.asGen))
}

object FiniteGen {


  private def apply[A](ga: Gen[A]): FiniteGen[A] = new FiniteGen[A] {
    override val asGen: Gen[A] = ga
  }

  /**
   * @return a finite Gen that generates one value from delegate Gen
   */
  def once[A](ga: Gen[A]): FiniteGen[A] = {
    var flag = true

    apply(Gen.delay {
      if (flag) {
        flag = false
        ga.sample.fold[Gen[A]](Gen.fail)(Gen.const)
      } else {
        Gen.fail
      }
    })
  }

  /**
   * @return a finite Gen that generates all values in xa and then fails
   */
  def allOf[A](xa: Iterable[A]): FiniteGen[A] = {
    val iterator = xa.iterator

    apply(Gen.delay {
      if (iterator.hasNext) {
        Gen.const(iterator.next())
      } else {
        Gen.fail
      }
    })
  }

  /**
   * @return a finite Gen that generates all iterables in gxa and all values in each iterable and then fails
   */
  def flatten[A](gxa: FiniteGen[Iterable[A]]): FiniteGen[A] = {
    var inner: Option[FiniteGen[A]] = gxa.sample.map(xa => allOf(xa))

    def loop: Gen[A] = {
      inner match {
        case None =>
          Gen.fail
        case Some(innerGen) =>
          innerGen.sample match {
            case Some(a) =>
              Gen.const(a)
            case None =>
              inner = gxa.sample.map(xa => allOf(xa))
              loop
          }
      }
    }

    apply(Gen.delay(loop))
  }

}
