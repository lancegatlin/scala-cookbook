package org.ldg.util

/**
 * A type class that represents an arrow monadic type F to monadic type G, where the G can defer or suspend execution
 * of F.
 * @tparam F the monadic type
 * @tparam G the monadic type that can handle deferred evaluation of F
 */
trait DeferredFunctionK[F[_], G[_]] {
  def apply[A](deferredFa: () => F[A]): G[A]
}
