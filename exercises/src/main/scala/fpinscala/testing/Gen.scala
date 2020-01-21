package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.state.RNG._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}
import scala.util.control.NonFatal

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

case class Prop(run: (MaxSize, TestCases, RNG) => Result) { self =>
  def &&(p: Prop): Prop =
    Prop { (maxSize, testCases, rng) =>
      run(maxSize, testCases, rng) match {
        case Passed             => p.run(maxSize, testCases, rng)
        case failure: Falsified => failure
      }
    }
  def ||(p: Prop): Prop =
    Prop { (maxSize, testCases, rng) =>
      run(maxSize, testCases, rng) match {
        case Passed       => Passed
        case _: Falsified => p.run(maxSize, testCases, rng)
      }
    }
}

object Prop {
  type MaxSize = Int
  type TestCases = Int
  type FailedCase = String
  type SuccessCount = Int

  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    override def isFalsified = false
  }
  final case class Falsified(failure: FailedCase,
                             successCount: SuccessCount) extends Result {
    override def isFalsified: Boolean = true
  }

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop =
    Prop { (_, testCases, rng) =>
      randomStream[A](gen)(rng)
        .take(testCases)
        .zipWith(Stream.from(0)) {
          case (a, i) =>
            try {
              if (f(a)) Passed
              else Falsified(a.toString(), i)
            } catch {
              case NonFatal(t) => Falsified(buildMsg(a, t), i)
            }
        }
        .find(_.isFalsified)
        .getOrElse(Passed)
    }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g.forSize)(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop =
    Prop { (maxSize, testCases, rng) =>
      val casesPerSize = (testCases + (maxSize - 1)) / maxSize
      Stream.from(0)
        .map(g)
        .map(forAll(_)(f))
        .map { p =>
          Prop { (maxSize, _, rng) =>
            p.run(maxSize, casesPerSize, rng)
          }
        }
        .take((testCases min maxSize) + 1)
        .toList
        .reduce(_ && _)
        .run(maxSize, testCases, rng)
    }

  def run(
    p: Prop,
    maxSize: Int = 100,
    testCases: Int = 100,
    rng: RNG = RNG.Simple(System.currentTimeMillis())
  ): Unit = 
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) => 
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed => 
        println(s"+ OK, passed $testCases tests")
    }

  private def randomStream[A](gen: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng) { rng =>
      Some(gen.sample.run(rng))
    }

  private def buildMsg[A](a: A, t: Throwable): String =
    s"test case: $a\n" +
    s"generated an exception: ${t.getMessage}\n" +
    s"stack trace:\n${t.getStackTrace().mkString("\n")}"

  object Samples {
    val smallInt = Gen.choose(-10,10)
    val maxProp = forAll(listOf1(smallInt)) { ns =>
      val max = ns.max
      !ns.exists(_ > max)
    }
    val sortedProp = forAll(listOf1(smallInt)) { ns =>
      val sorted = ns.sorted
      sorted.head == sorted.min && sorted.last == sorted.max
    }
  }
}

case class Gen[+A](sample: Rand[A]) { self =>

  def flatMap[B](f: A => Gen[B]): Gen[B] = 
    Gen(sample.flatMap(a => f(a).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(Gen.listOfN(_, this))

  def unsized: SGen[A] = SGen(_ => self)
}

object Gen {
  import fpinscala.state.RNG
  import fpinscala.state.RNG.Rand

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  val boolean: Gen[Boolean] = Gen(RNG.boolean)

  val double: Gen[Double] = Gen(RNG.double)

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(RNG.choose(start, stopExclusive))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    sequence(List.fill(n)(g))

  def sequence[A](gs: List[Gen[A]]): Gen[List[A]] =
    Gen(State.sequence(gs.map(_.sample)))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if(b) g1 else g2)

  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = {
    val (gen1, weight1) = g1
    val (gen2, weight2) = g2
    val totalWeight = weight1 + weight2
    val ratio1 = weight1 / totalWeight
    double.flatMap { d =>
      if (d < ratio1) gen1
      else gen2
    }
  }

  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(listOfN(_, g))

  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    SGen(size => listOfN(size max 1, g))
}

case class SGen[+A](forSize: Int => Gen[A]) {

  def flatMap[B](f: A => SGen[B]): SGen[B] =
    SGen { size =>
      forSize(size).flatMap { a =>
        f(a).forSize(size)
      }
    }
}