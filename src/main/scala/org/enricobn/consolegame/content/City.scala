package org.enricobn.consolegame.content

import java.time.LocalDate

import scala.collection.mutable.ArrayBuffer

/**
  * Created by enrico on 1/8/17.
  */
case class City(name: String, founded: LocalDate, statistics: Statistics) {

  override def toString: String =
    "Name: " + name + "\n"
    "Founded: " + founded + "\n"
    "Statistics:\n" + statistics.last()
}

case class StatisticEntry(population: Int, employed: Int) {

  override def toString: String =
    "Population: " + population + "\n"
    "Employed: " + employed
}

class Statistics(initial: StatisticEntry) {
  private val history = new ArrayBuffer[StatisticEntry]()
  add(initial)

  def add(entry: StatisticEntry): Unit = {
    history += entry
  }

  def last() = history.last

}