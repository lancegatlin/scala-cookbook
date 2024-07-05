package org.ldg.util

object AnyUnfoldExt {
  implicit class OrgLdgUtilAnyUnfoldExt[A](val self: A) extends AnyVal {
    def tuple[B,C](b: A => B, c: A => C): (B,C) = (b(self), c(self))
    def tuple[B,C,D](b: A => B, c: A => C, d: A => D): (B,C,D) = (b(self), c(self), d(self))
    def tuple[B,C,D,E](b: A => B, c: A => C, d: A => D, e: A => E): (B,C,D,E) = (b(self), c(self), d(self), e(self))
    def tuple[B,C,D,E,F](b: A => B, c: A => C, d: A => D, e: A => E, f: A => F): (B,C,D,E,F) = (b(self), c(self), d(self), e(self), f(self))
    def tuple[B,C,D,E,F,G](b: A => B, c: A => C, d: A => D, e: A => E, f: A => F, g: A => G): (B,C,D,E,F,G) = (b(self), c(self), d(self), e(self), f(self), g(self))
  }

  object ExampleUsage {
    val s = "test"

    val tuple2: (Char, Int) = s.tuple(_(0), _.length)
    val tuple3: (Char, Int, Int) = s.tuple(_(0), _.length, _.length)

    case class Test(i: Int, s: String)

    val values: Seq[Test] = ???

    values.map(t => (t.i, t.s)).toMap
    values.map(_.tuple(_.i, _.s)).toMap
  }
}
