package org.enricobn.consolegame.content

import org.enricobn.buyandsell.content.{CityMap, Hospital, PoliceStation, Position}
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by enrico on 1/8/17.
  */
class CityMapSpec extends FlatSpec with MockFactory with Matchers {

  "simple" should "be fine" in {
    val example = new CityMap(3, 3)
    assert(example.add(Position(0, 0), PoliceStation(1, 1)))
    assert(example.add(Position(2, 2), Hospital(1, 1)))

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
    val example = new CityMap(3, 3)
    assert(example.add(Position(0, 0), PoliceStation(2, 1)))
    assert(example.add(Position(2, 2), Hospital(1, 1)))

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
    val example = new CityMap(3, 3)
    assert(example.add(Position(0, 0), PoliceStation(1, 2)))
    assert(example.add(Position(2, 2), Hospital(1, 1)))

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
    val example = new CityMap(3, 3)
    assert(!example.add(Position(0, 0), PoliceStation(1, 4)))
  }

  "overlap" should "not be fine" in {
    val example = new CityMap(3, 3)
    assert(example.add(Position(2, 2), Hospital(1, 1)))
    assert(!example.add(Position(2, 0), PoliceStation(1, 3)))
  }

}
