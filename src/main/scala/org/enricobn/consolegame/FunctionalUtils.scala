package org.enricobn.consolegame

import scala.collection.GenTraversableOnce

object FunctionalUtils {

  def lift[T,TL,TR](xs: GenTraversableOnce[(T, Either[TL,TR])]) : Either[TL,List[(T,TR)]] =
    xs.foldRight(Right(List.empty[(T,TR)]) : Either[TL,List[(T,TR)]]) { (value, result) => {
      result match {
        case Left(_) => result
        case Right(r) =>
          value match {
            case (t,Left(l)) => Left(l)
            case (t,Right(r1)) => Right( (t,r1) :: r)
          }
      }
    }}

  def lift[T,T1](xs: GenTraversableOnce[(T, Option[T1])]) : Option[List[(T,T1)]] =
    xs.foldRight(Some(List.empty) : Option[List[(T,T1)]])((value, result) => {
      result match {
        case Some(l) =>
          value match {
            case (t, Some(v)) => Some((t,v) :: l)
            case _ => None
          }
        case _ => None
      }
    })

  def allSome[T,T1](xs: GenTraversableOnce[(T, Option[T1])]) : List[(T,T1)] =
    xs.foldRight(List.empty : List[(T,T1)])((value, result) => {
      value match {
        case (t, Some(v)) => (t,v) :: result
        case _ => result
      }
    })

}
