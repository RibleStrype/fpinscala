package fpinscala.state
import scala.annotation.tailrec


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[A] = State[RNG, A]

  val int: Rand[Int] = State(_.nextInt)

  def unit[A](a: A): Rand[A] =
    State(rng => (a, rng))

  val nonNegativeInt: Rand[Int] = int.map { n =>
    if (n < 0) -(n + 1) else n
  }

  val double: Rand[Double] = nonNegativeInt.map { n =>
    n / (Int.MaxValue.toDouble + 1)
  }

  val intDouble: Rand[(Int, Double)] = int.map2(double)((_, _))

  val doubleInt: Rand[(Double, Int)] = intDouble.map { case (i, d) => (d, i) }

  val double3: Rand[(Double, Double, Double)] =
    for {
      d1 <- double
      d2 <- double
      d3 <- double
    } yield (d1, d2, d3)

  def ints(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List.empty[A]))(_.map2(_)(_ :: _))

  def nonNegativeLessThan(n: Int): Rand[Int] =
    nonNegativeInt.flatMap { i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }

  def choose(start: Int, stopExclusive: Int): Rand[Int] = {
    require(stopExclusive > start, "stopExclusive must be greater thant start")
    nonNegativeLessThan(stopExclusive - start) map (_ + start)
  }
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    State { s => 
      val (a, s2) = run(s)
      (f(a), s2)
    }
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    State { s =>
      val (a, s2) = run(s)
      val (b, s3) = sb.run(s2)
      (f(a, b), s3)
    }
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State { s =>
      val (a ,s2) = run(s)
      f(a).run(s2)
    }
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  def unit[S, A](a: A): State[S, A] =
    State((a, _))

  def sequence[S, A](states: List[State[S, A]]): State[S, List[A]] =
    states.foldRight[State[S, List[A]]](unit(List.empty))(_.map2(_)(_ :: _))
    
  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    for {
      _       <- sequence(inputs.map(i => modify(handleInput(i))))
      machine <- get
    } yield (machine.coins, machine.candies)

  def handleInput(input: Input)(machine: Machine): Machine =
    input match {
      case Coin if machine.locked && machine.candies > 0 => 
        machine.copy(locked = false, coins = machine.coins + 1)
      case Turn if !machine.locked =>
        machine.copy(locked = true, candies = machine.candies - 1)
      case _ => machine
    }
}
