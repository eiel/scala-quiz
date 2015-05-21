package com.chatwork.quiz.collection

import com.chatwork.quiz.{MySome, MyNone, MyOption}
import com.chatwork.quiz.collection.MyList

sealed trait MyList[+A] {

  // Easy
  def length: Int = foldLeft(0) {(acc,x) => 1 + acc}

  // Normal
  def foldLeft[B](z: B)(f: (B, A) => B): B = this match {
    case MyNil => z
    case MyCons(a, as) => as.foldLeft(f(z,a))(f)
  }

  // 難易度選択制
  // Normal: 条件 - 特にありません、気の向くままに実装してください。
  // Hard:   条件 - foldLeftを使って実装してください。
  def foldRight[B](z: B)(f: (A, B) => B): B = reverse.foldLeft(z){ (acc,x) =>
    f(x, acc)
  }
  //{
    //this match {
    //case MyNil => z
    //case MyCons(a, as) => f(a, as.foldRight(z)(f))
  //}

  // Normal
  // scalastyle:off
  def ::[B >: A](b: B): MyList[B] = MyCons(b, this)
  // scalastyle:on

  // Normal
  def reverse: MyList[A] = foldLeft(MyList.empty[A]){(xs,x) => x :: xs}

  // Normal
  // scalastyle:off
  def ++[B >: A](b: MyList[B]): MyList[B] = foldRight(b)(_ :: _)

  // scalastyle:on

  // Normal
  def map[B](f: A => B): MyList[B] = foldRight(MyList.empty[B]) { (a,acc) => f(a) :: acc }

  // Normal
  def flatMap[B](f: A => MyList[B]): MyList[B] = map(f).foldRight(MyList.empty[B]) { (a,acc) => a ++ acc }

  // Normal
  def filter(f: A => Boolean): MyList[A] = foldRight(MyList.empty[A]) { (a,acc) =>
    if (f(a)) a :: acc
    else acc
  }

  // Normal: 条件 - filterと同様の実装でも構いません。
  // Hard:   条件 - 中間リストを生成しないように実装してください。
  def withFilter(f: A => Boolean): MyList[A] = filter(f)

  // Normal
  def find(f: A => Boolean): MyOption[A] = withFilter(f).headOpt

  def headOpt(): MyOption[A] =
    this match {
      case MyNil => MyNone
      case MyCons(a,_) => MySome(a)
  }

  // Normal
  def startsWith[B >: A](prefix: MyList[B]): Boolean = (this,prefix) match {
    case (_,MyNil) => true
    case (MyCons(a,as),MyCons(b,bs)) => if (a == b) as.startsWith(bs) else false
    case (_,_) => false
  }

}

case object MyNil extends MyList[Nothing]

case class MyCons[+A](head: A, tail: MyList[A]) extends MyList[A]

object MyList {

  // Easy
  def empty[A]: MyList[A] = MyNil

  // Normal
  def apply[A](as: A*): MyList[A] = as.foldRight(MyList.empty[A]) { (x,acc) => MyCons(x,acc) }

}
