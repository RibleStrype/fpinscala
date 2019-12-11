package fpinscala.errorhandling

sealed trait Validation[+E, +A] {
    def map[B](f: A => B): Validation[E, B] = this match {
        case e@Errors(_) => e
        case Success(a)  => Success(f(a))
    }

    def orElse[EE >: E, B >: A](b: => Validation[EE, B]): Validation[EE, B] =
        this match {
            case Errors(_)    => b
            case s@Success(_) => s
        }

    def map2[EE >: E, B, C](b: Validation[EE, B])(f: (A, B) => C): Validation[EE, C] =
        (this, b) match {
            case (Errors(es1), Errors(es2)) => Errors(es1 ++ es2)
            case (Errors(es), Success(_))   => Errors(es)
            case (Success(_), Errors(es))   => Errors(es)
            case (Success(a), Success(b))   => Success(f(a, b))
        }
}

final case class Errors[E](errors: List[E]) extends Validation[E, Nothing]
final case class Success[A](value: A) extends Validation[Nothing, A]

object Validation {
    
    def apply[E, A](e: Either[E, A]): Validation[E, A] = e match {
        case Left(err) => Errors(List(err))
        case Right(a)  => Success(a)
    }

    def traverse[E,A,B](es: List[A])(f: A => Validation[E, B]): Validation[E, List[B]] =
        es.foldRight(Success(List.empty): Validation[E, List[B]]) { (e, acc) =>
            f(e).map2(acc)(_ :: _)
        }

    def sequence[E,A](es: List[Validation[E,A]]): Validation[E,List[A]] =
        traverse(es)(identity)
}

object ValidationTest {
    final case class Person(name: Name, age: Age)
    final case class Name(val value: String)
    final case class Age(val value: Int)

    def mkName(name: String): Either[String, Name] =
        if (name == "" || name == null) Left("Name is empty.")
        else                            Right(Name(name))

    def mkAge(age: Int): Either[String, Age] = 
        if (age < 0) Left("Age is out of range.") 
        else         Right(Age(age))

    def mkPerson(name: String, age: Int): Validation[String, Person] =
        Validation(mkName(name)).map2(Validation(mkAge(age)))(Person(_, _))
}