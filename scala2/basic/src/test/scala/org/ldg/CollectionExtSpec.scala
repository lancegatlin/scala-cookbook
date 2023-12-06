package org.ldg

import org.ldg.CollectionExt._
import org.scalatest.Inside
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class CollectionExtSpec extends AnyWordSpec with Matchers with Inside {
  "groupWeightedMaxBy" should {
    "return empty iterator if input is empty" in {
      List.empty[Int].groupWeightedMaxBy( 7 )( _.toLong ).toList shouldBe Nil
    }

    "return a single group if input has one element" in {
      List( 1 ).groupWeightedMaxBy( 7 )( _.toLong ).toList shouldBe List( List( 1 ) )
    }

    "return an iterator of the same type of collection as its input" in {
      val result = Vector( 1 ).groupWeightedMaxBy( 7 )( _.toLong ).toList
      result.head.isInstanceOf[Vector[Int]] shouldBe true
      result shouldBe List( Vector( 1 ) )
    }

    "group consecutive elements by weight into subgroups whose total weight is no more than max weight" in {
      inside(
        List( 1, 2, 3 ).groupWeightedMaxBy( 7 )( _.toLong ).toList
      ) { result =>
        result.size shouldBe 1
        result( 0 ) shouldBe List( 1, 2, 3 )
      }
      inside(
        List( 1, 2, 3 ).groupWeightedMaxBy( 6 )( _.toLong ).toList
      ) { result =>
        result.size shouldBe 1
        result( 0 ) shouldBe List( 1, 2, 3 )
      }
      inside(
        List( 1, 2, 3 ).groupWeightedMaxBy( 5 )( _.toLong ).toList
      ) { result =>
        result.size shouldBe 2
        result( 0 ) shouldBe List( 1, 2 )
        result( 1 ) shouldBe List( 3 )
      }
      inside(
        List( 1, 2, 3 ).groupWeightedMaxBy( 4 )( _.toLong ).toList
      ) { result =>
        result.size shouldBe 2
        result( 0 ) shouldBe List( 1, 2 )
        result( 1 ) shouldBe List( 3 )
      }
      inside(
        List( 1, 2, 3 ).groupWeightedMaxBy( 3 )( _.toLong ).toList
      ) { result =>
        result.size shouldBe 2
        result( 0 ) shouldBe List( 1, 2 )
        result( 1 ) shouldBe List( 3 )
      }
      inside(
        List( 1, 2, 3 ).groupWeightedMaxBy( 2 )( _.toLong ).toList
      ) { result =>
        result.size shouldBe 3
        result shouldBe List( List( 1 ), List( 2 ), List( 3 ) )
      }
    }

    "place a single element whose weight is equal to or greater than max weight into its own group" in {
      inside(
        List( 1, 2, 7, 1 ).groupWeightedMaxBy( 7 )( _.toLong ).toList
      ) { result =>
        result.size shouldBe 3
        result( 0 ) shouldBe List( 1, 2 )
        result( 1 ) shouldBe List( 7 )
        result( 2 ) shouldBe List( 1 )
      }
      inside(
        List( 1, 2, 6, 1 ).groupWeightedMaxBy( 7 )( _.toLong ).toList
      ) { result =>
        result.size shouldBe 2
        result( 0 ) shouldBe List( 1, 2 )
        result( 1 ) shouldBe List( 6, 1 )
      }
      inside(
        List( 1, 2, 8, 1 ).groupWeightedMaxBy( 7 )( _.toLong ).toList
      ) { result =>
        result.size shouldBe 3
        result( 0 ) shouldBe List( 1, 2 )
        result( 1 ) shouldBe List( 8 )
        result( 2 ) shouldBe List( 1 )
      }
    }
  }
}
