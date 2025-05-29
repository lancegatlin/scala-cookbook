package org.ldg.effect

/**
 * An exception that indicates an Async effect type's execution plan evaluation (e.g. FuturePlan) was canceled.
 * @param message the message describing the cancellation reason
 */
class CanceledEvalException( message: String ) extends RuntimeException( message )
