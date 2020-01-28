package fpinscala.parsing

import language.higherKinds

trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]
  def or[A](p1: Parser[A], p2: Parser[A]): Parser[A]
  def manyN[A](n: Int, p: Parser[A]): Parser[List[A]]
  def many[A](p: Parser[A]): Parser[List[A]]
  def map[A, B](p: Parser[A])(f: A => B): Parser[B]
  def slice[A](p: Parser[A]): Parser[String]
  def product[A, B](p1: Parser[A], p2: Parser[B]): Parser[(A, B)]

  def char(c: Char): Parser[Char] =
    string(c.toString()).map(_.charAt(0))
  def succeed[A](a: A): Parser[A] =
    string("").map(_ => a)
  def map2[A,B,C](p: Parser[A], p2: Parser[B])(f: (A,B) => C): Parser[C] =
    product(p, p2).map(f.tupled)
  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _)

  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[A]): ParserOps[A] =
    ParserOps(f(a))

  case class ParserOps[A](p: Parser[A]) {
    def or[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def many: Parser[List[A]] = self.many(p)
    def manyN(n: Int): Parser[List[A]] = self.manyN(n, p)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
  }

  object Laws {
    import fpinscala.testing._

    def equal[A](p1: Parser[A], p2: Parser[A])(g: Gen[String]): Prop =
      Prop.forAll(g)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(g: Gen[String]): Prop =
      equal(p, p.map(identity))(g)

    def succeedLaw[A](ga: Gen[A], gs: Gen[String]): Prop =
      Prop.forAll(ga ** gs) {
        case (a, s) => run(succeed(a))(s) == Right(a)
      }
  }
}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  /* Returns the line corresponding to this location */
  def currentLine: String = 
    if (input.length > 1) input.lines.drop(line-1).next
    else ""
}

case class ParseError(stack: List[(Location,String)] = List(),
                      otherFailures: List[ParseError] = List()) {
}