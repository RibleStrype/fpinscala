package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

trait Prop { self =>
  def check: Boolean
  def &&(p: Prop): Prop = new Prop {
    def check: Boolean = self.check && p.check
  }
}

object Prop {
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

object Gen {
  import fpinscala.state.RNG
  import fpinscala.state.RNG.Rand

  def unit[A](a: => A): Gen[A] = RandGen(State.unit(a))

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    RandGen(RNG.choose(start, stopExclusive))

  case class RandGen[A](sample: Rand[A]) extends Gen[A]
}

trait Gen[A] {
  def map[A,B](f: A => B): Gen[B] = ???
  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
}

trait SGen[+A] {

}

