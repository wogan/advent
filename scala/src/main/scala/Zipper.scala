package dev.wogan.advent.scala

import cats.data.NonEmptyList
import cats.syntax.all.*
import cats.{Comonad, Foldable, Reducible, Show}

import scala.annotation.tailrec

case class Zipper[A](left: List[A], focus: A, right: List[A]) {

  def moveRight: Option[Zipper[A]] =
    right.headOption.map { nextFocus =>
      Zipper(focus :: left, nextFocus, right.tail)
    }

  def moveLeft: Option[Zipper[A]] =
    left.headOption.map { nextFocus =>
      Zipper(left.tail, nextFocus, focus :: right)
    }

  def toList: List[A] =
    left reverse_::: focus +: right

  def delete: Option[Zipper[A]] =
    right.headOption.map { nextFocus =>
      Zipper(left, nextFocus, right.tail)
    }.orElse(left.headOption.map { nextFocus =>
      Zipper(left.tail, nextFocus, right)
    })
  
  def duplicate: Zipper[Zipper[A]] =
    Zipper(duplicateLeft(identity), this, duplicateRight(identity))

  def duplicateRight[B](f: Zipper[A] => B): List[B] =
    Zipper.unfold(this)(_.moveRight.map(z => f(z) -> z))

  def duplicateLeft[B](f: Zipper[A] => B): List[B] =
    Zipper.unfold(this)(_.moveLeft.map(z => f(z) -> z))

}

object Zipper {
  def fromList[A](items: List[A]): Option[Zipper[A]] =
    items.headOption.map(Zipper(List.empty, _, items.tail))

  def fromListUnsafe[A](items: List[A]): Zipper[A] =
    Zipper(List.empty, items.head, items.tail)

  def fromNonEmptyList[A](nel: NonEmptyList[A]): Zipper[A] =
    Zipper(List.empty, nel.head, nel.tail)

  def unfold[A, B](initial: A)(f: A => Option[(B, A)]): List[B] =
    f(initial) match {
      case Some((b, a)) => b :: unfold(a)(f)
      case None => List.empty
    }

  given Comonad[Zipper] with
    override def extract[A](w: Zipper[A]): A =
      w.focus

    override def map[A, B](fa: Zipper[A])(f: A => B): Zipper[B] =
      Zipper(fa.left.map(f), f(fa.focus), fa.right.map(f))

    override def coflatMap[A, B](fa: Zipper[A])(f: Zipper[A] => B): Zipper[B] =
      Zipper(fa.duplicateLeft(f), f(fa), fa.duplicateRight(f))

  given [A](using s: Show[A]): Show[Zipper[A]] with
    override def show(z: Zipper[A]): String = {
      val left = z.left.reverse.mkString_(", ")
      val right = z.right.mkString_(", ")
      show"$left -> ${z.focus} <- $right"
    }

}
