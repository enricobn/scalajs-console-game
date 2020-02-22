package org.enricobn.buyandsell.content

import org.enricobn.vfs.IOError

import scala.util.Try

object GoodEnum extends Enumeration {
  type GoodEnum = Value

  val gold: GoodEnum = Value("gold")
  val silver: GoodEnum = Value("silver")
  val bronze: GoodEnum = Value("bronze")

  def withNameE(value: String) : Either[IOError, GoodEnum] =
    Try(GoodEnum.withName(value)).toOption.toRight(IOError(s"Cannot find enum for '$value'."))

}
