package eu.ddd.transportTycoon

import org.scalatest.flatspec.AnyFlatSpec

class TransportTycoonSpec extends AnyFlatSpec{

  "a cargo B" should "take 5 hours" in {
    val time = TransportTycoon.calculateTime("B")
    assert(time == 5)
  }

  "a cargo A" should "take 5 hours" in {
    val time = TransportTycoon.calculateTime("A")
    assert(time == 5)
  }

  "a cargo list BB" should "take 5 hours" in {
    val time = TransportTycoon.calculateTime("BB")
    assert(time == 5)
  }

  "a cargo list ABB" should "take 7 hours" in {
    val time = TransportTycoon.calculateTime("ABB")
    assert(time == 7)
  }

  "a cargo list AABABBAB" should "take 29 hours" in {
    val time = TransportTycoon.calculateTime("AABABBAB")
    assert(time == 29)
  }

}
