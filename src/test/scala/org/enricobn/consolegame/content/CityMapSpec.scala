package org.enricobn.consolegame.content

import org.enricobn.buyandsell.content.{CityMap, Hospital, PoliceStation, Position}
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by enrico on 1/8/17.
  */
class CityMapSpec extends FlatSpec with MockFactory with Matchers {

  "simple" should "be fine" in {
    var example = CityMap(3, 3)
    example = example.add(Position(0, 0), PoliceStation(1, 1)).right.get
    example = example.add(Position(2, 2), Hospital(1, 1)).right.get

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
    example = example.add(Position(0, 0), PoliceStation(2, 1)).right.get
    example = example.add(Position(2, 2), Hospital(1, 1)).right.get

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
    example = example.add(Position(0, 0), PoliceStation(1, 2)).right.get
    example = example.add(Position(2, 2), Hospital(1, 1)).right.get

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
    example = example.add(Position(2, 2), Hospital(1, 1)).right.get
    assert(example.add(Position(2, 0), PoliceStation(1, 3)).isLeft)
  }

}
