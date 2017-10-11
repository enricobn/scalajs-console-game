package org.enricobn.buyandsell.content

import java.time.LocalDate

/**
  * Created by enrico on 1/8/17.
  */
case class City(name: String, founded: LocalDate, statistics: Statistics) {

  override def toString: String =
    "Name: " + name + "\n" +
    "Founded: " + founded + "\n" +
    "Statistics:\n" + statistics.last
}

case class StatisticEntry(population: Int, employed: Int) {

  override def toString: String =
    "Population: " + population + "\n" +
    "Employed: " + employed
}

case class Statistics(history: Seq[StatisticEntry]) {

  def add(entry: StatisticEntry): Statistics = {
    Statistics(history :+ entry)
  }

  def last: StatisticEntry = history.last

}