package org.ldg.test

import org.scalacheck.Gen

import scala.reflect.ClassTag

object GenExt {
  implicit class GentExt[A](val self: Gen[A]) extends AnyVal {
    def sampleOrFail(implicit aClassTag: ClassTag[A]): A = self.sample.getOrElse(
      throw new RuntimeException(s"failed to sample Gen[${aClassTag.getClass.getSimpleName}]")
    )
    def toLazyList(implicit aClassTag: ClassTag[A]): LazyList[A] =
      LazyList.continually(sampleOrFail)
  }
}
