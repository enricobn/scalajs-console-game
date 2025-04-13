package org.enricobn.buyandsell.content

import org.scalamock.matchers.Matchers
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec

/**
  * Created by enrico on 1/8/17.
  */
class CityMapSpec extends AnyFlatSpec with MockFactory with Matchers {

  "simple" should "be fine" in {
    var example = CityMap(3, 3)
    example = example.add(Position(0, 0), PoliceStation(1, 1)).toOption.get
    example = example.add(Position(2, 2), Hospital(1, 1)).toOption.get

    val expected =
      """+-+-+-+
        &|P| | |
        &+-+-+-+
        &| | | |
        &+-+-+-+
        &| | |H|
        &+-+-+-+
        &"""
      .stripMargin('&')

    assert(example.toString == expected)
  }

  "size 2" should "be fine" in {
    var example = CityMap(3, 3)
    example = example.add(Position(0, 0), PoliceStation(2, 1)).toOption.get
    example = example.add(Position(2, 2), Hospital(1, 1)).toOption.get

    val expected =
      """+-+-+-+
        &|P|P| |
        &+-+-+-+
        &| | | |
        &+-+-+-+
        &| | |H|
        &+-+-+-+
        &"""
        .stripMargin('&')

    assert(example.toString == expected)
  }

  "size 2 vertical" should "be fine" in {
    var example = CityMap(3, 3)
    example = example.add(Position(0, 0), PoliceStation(1, 2)).toOption.get
    example = example.add(Position(2, 2), Hospital(1, 1)).toOption.get

    val expected =
      """+-+-+-+
        &|P| | |
        &+-+-+-+
        &|P| | |
        &+-+-+-+
        &| | |H|
        &+-+-+-+
        &"""
        .stripMargin('&')

    assert(example.toString == expected)
  }

  "size 4 vertical" should "not be fine" in {
    val example = CityMap(3, 3)

    assert(example.add(Position(0, 0), PoliceStation(1, 4)).isLeft)
  }

  "overlap" should "not be fine" in {
    var example = CityMap(3, 3)
    example = example.add(Position(2, 2), Hospital(1, 1)).toOption.get
    assert(example.add(Position(2, 0), PoliceStation(1, 3)).isLeft)
  }

}
