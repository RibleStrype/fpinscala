package fpinscala
package applicative

import monads.Functor
import state._
import State._
//import StateUtil._ // defined at bottom of this file
import monoids._
import language.higherKinds
import language.implicitConversions
import scala.util.control.NonFatal

trait Applicative[F[_]] extends Functor[F] { self =>

  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(apply(unit(f.curried))(fa))(fb)

  def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)((f, a) => f(a))

  def unit[A](a: => A): F[A]

  def map[A,B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    traverse(fas)(identity)

  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List.empty[B]))((a, bs) => map2(f(a), bs)(_ :: _))

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  def factor[A,B](fa: F[A], fb: F[B]): F[(A,B)] =
    map2(fa, fb)((_, _))

  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = 
    ms.foldRight(unit(List.empty[A])) { (a, mas) =>
      map2(f(a), mas) {
        case (true, as) => a :: as 
        case (false, as) => as
      }
    }

  def map3[A,B,C,D](fa: F[A], 
                    fb: F[B],
                    fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(apply(apply(unit(f.curried))(fa))(fb))(fc)

  def map4[A,B,C,D,E](fa: F[A],
                      fb: F[B], 
                      fc: F[C],
                      fd: F[D])(f: (A, B, C, D) => E): F[E] =
    apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)

  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] =
    new Applicative[({type f[x] = (F[x], G[x])})#f] {
      override def unit[A](a: => A): (F[A], G[A]) = 
        (self.unit(a), G.unit(a))
      override def apply[A, B](fab: (F[A => B], G[A => B]))(fa: (F[A], G[A])): (F[B], G[B]) = 
        (self.apply(fab._1)(fa._1), G.apply(fab._2)(fa._2))
    }

  def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] =
    new Applicative[({type f[x] = F[G[x]]})#f] {
      override def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))
      override def apply[A, B](fab: F[G[A => B]])(fa: F[G[A]]): F[G[B]] =
        self.map2(fab, fa)(G.apply(_)(_))
    }

  def sequenceMap[K,V](ofa: Map[K,F[V]]): F[Map[K,V]] =
    ofa.foldLeft(unit(Map.empty[K, V])) { (fm, kv) =>
      map2(fm, kv._2)((m, v) => m.updated(kv._1, v))
    }
}

case class Tree[+A](head: A, tail: List[Tree[A]])

trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B] = join(map(ma)(f))

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)

  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  override def apply[A,B](mf: F[A => B])(ma: F[A]): F[B] =
    flatMap(mf)(f => map(ma)(a => f(a)))
}

object Monad {
  def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] =
    new Monad[({type f[x] = Either[E, x]})#f] {
      override def unit[A](a: => A): Either[E,A] = Right(a)
      override def flatMap[A, B](ma: Either[E,A])(f: A => Either[E,B]): Either[E,B] = 
        ma flatMap f
    }

  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    override def map[A, B](fa: State[S,A])(f: A => B): State[S,B] = 
      fa map f
    override def flatMap[A,B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }

  def composeM[F[_],N[_]](implicit F: Monad[F], N: Monad[N], T: Traverse[N]):
    Monad[({type f[x] = F[N[x]]})#f] = 
      new Monad[({type f[x] = F[N[x]]})#f] {
        override def unit[A](a: => A): F[N[A]] = F.unit(N.unit(a))
        override def flatMap[A, B](ma: F[N[A]])(f: A => F[N[B]]): F[N[B]] = 
          F.flatMap(ma) { na =>
            val fnnb = T.traverse(na)(f)
            F.map(fnnb)(N.join)
          }
      }
}

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E] = Vector.empty)
  extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]


object Applicative {

  val streamApplicative = new Applicative[Stream] {

    def unit[A](a: => A): Stream[A] =
      Stream.continually(a) // The infinite, constant stream

    override def map2[A,B,C](a: Stream[A], b: Stream[B])( // Combine elements pointwise
                    f: (A,B) => C): Stream[C] =
      a zip b map f.tupled
  }

  implicit def validationApplicative[E]: Applicative[({type f[x] = Validation[E,x]})#f] =
    new Applicative[({type f[x] = Validation[E,x]})#f] {
      override def unit[A](a: => A): Validation[E,A] = Success(a)
      override def map2[A, B, C](fa: Validation[E,A], fb: Validation[E,B])(f: (A, B) => C): Validation[E,C] =
        (fa, fb) match {
          case (Success(a), Success(b)) => Success(f(a, b))
          case (f@Failure(_, _), Success(_)) => f
          case (Success(_), f@Failure(_, _)) => f
          case (Failure(lh, lt), Failure(rh, rt)) => Failure(lh, (lt :+ rh) ++ rt)
        }
    }

  type Const[A, B] = A

  implicit def monoidApplicative[M](M: Monoid[M]) =
    new Applicative[({ type f[x] = Const[M, x] })#f] {
      def unit[A](a: => A): M = M.zero
      override def apply[A,B](m1: M)(m2: M): M = M.op(m1, m2)
    }

  type Id[A] = A

  implicit val idApplicative: Applicative[Id] =
    new Applicative[Id] {
      override def unit[A](a: => A): Id[A] = a
      override def apply[A, B](fab: Id[A => B])(fa: Id[A]): Id[B] = 
        fab(fa)
    }
}

object Form {
  import java.util.Date

  final case class WebForm(name: String, birthDate: Date, phoneNumber: String)

  def validName(name: String): Validation[String, String] =
    if (!name.isEmpty) Success(name)
    else Failure("Name cannot be empty")

  def validBirthDate(birthdate: String): Validation[String, Date] =
    try {
      import java.text._
      Success((new SimpleDateFormat("yyyy-MM-dd")).parse(birthdate))
    } catch {
      case NonFatal(_) => Failure("Birthdate must be in format yyyy-MM-dd")
    }

  def validPhone(phoneNumber: String): Validation[String, String] = 
    if (phoneNumber.matches("[0-9]{10}"))
      Success(phoneNumber)
    else 
    Failure("Phone number must be 10 digits")

  type StringValidation[A] = Validation[String, A]

  def validWebForm(name: String, birthdate: String, phone: String)(implicit A: Applicative[StringValidation]): Validation[String, WebForm] =
    A.map3(
      validName(name),
      validBirthDate(birthdate),
      validPhone(phone),
    )(WebForm(_, _, _))
}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] { self =>
  def traverse[G[_]:Applicative,A,B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence(map(fa)(f))
  def sequence[G[_]:Applicative,A](fma: F[G[A]]): G[F[A]] =
    traverse(fma)(ma => ma)

  import Applicative._

  def map[A,B](fa: F[A])(f: A => B): F[B] =
    traverse[Id, A, B](fa)(f)

  override def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    traverse[({type f[x] = Const[B,x]})#f,A,Nothing](
      as)(f)(monoidApplicative(mb))

  def traverseS[S,A,B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({type f[x] = State[S,x]})#f,A,B](fa)(f)(Monad.stateMonad)

  def mapAccum[S,A,B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) => (for {
      s1 <- get[S]
      (b, s2) = f(a, s1)
      _  <- set(s2)
    } yield b)).run(s)

  override def toList[A](fa: F[A]): List[A] =
    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  def reverse[A](fa: F[A]): F[A] =
    mapAccum(fa, toList(fa).reverse)((_, as) => (as.head, as.tail))._1

  override def foldLeft[A,B](fa: F[A])(z: B)(f: (B, A) => B): B =
    mapAccum(fa, z) { (a, s) => ((), f(s, a)) }._2

  def fuse[G[_],H[_],A,B](fa: F[A])(f: A => G[B], g: A => H[B])
                         (implicit G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) = {
    implicit val X = G.product(H)
    traverse[({type f[x] = (G[x], H[x])})#f, A, B](fa) { a =>
      (f(a), g(a))
    }
  }

  def compose[G[_]](implicit G: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] =
    new Traverse[({type f[x] = F[G[x]]})#f] {
      override def traverse[H[_]: Applicative, A, B](fa: F[G[A]])(f: A => H[B]): H[F[G[B]]] = 
        self.traverse[H, G[A], G[B]](fa) { ga =>
          G.traverse(ga) { a =>
            f(a)
          }
        }
      }
}

object Traverse {
  val listTraverse: Traverse[List] =
    new Traverse[List] {
      override def traverse[G[_], A, B](fa: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] = 
        G.traverse(fa)(f)
    }

  val optionTraverse: Traverse[Option] =
    new Traverse[Option] {
      override def traverse[G[_], A, B](fa: Option[A])(f: A => G[B])(implicit G: Applicative[G]): G[Option[B]] = 
        fa match {
          case None => G.unit(None)
          case Some(a) => G.map(f(a))(Some(_))
        }
    }

  val treeTraverse: Traverse[Tree] =
    new Traverse[Tree] {
      override def traverse[G[_], A, B](fa: Tree[A])(f: A => G[B])(implicit G: Applicative[G]): G[Tree[B]] = 
        G.map2(f(fa.head), G.traverse(fa.tail)(traverse(_)(f)))(Tree(_, _))
    }
}

// The `get` and `set` functions on `State` are used above,
// but aren't in the `exercises` subproject, so we include
// them here
object StateUtil {

  def get[S]: State[S, S] =
    State(s => (s, s))

  def set[S](s: S): State[S, Unit] =
    State(_ => ((), s))
}
