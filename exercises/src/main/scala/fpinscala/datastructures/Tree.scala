package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  def size(tree: Tree[_]): Int = tree match {
    case Leaf(_)      => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(n)      => n
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth(tree: Tree[_]): Int = tree match {
    case Leaf(_)      => 0
    case Branch(l, r) => (depth(l) max depth(r)) + 1
  }

  def map[A,B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(a)      => Leaf(f(a))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A,B](tree: Tree[A])(onLeaf: A => B, onBranch: (B, B) => B): B =
    tree match {
      case Leaf(a)      => onLeaf(a)
      case Branch(l, r) => onBranch(fold(l)(onLeaf, onBranch), fold(r)(onLeaf, onBranch))
    }

  def sizeViaFold(tree: Tree[_]): Int =
    fold(tree)(_ => 1, (l: Int, r: Int) => l + r)

  def maximumViaFold(tree: Tree[Int]): Int =
    fold[Int, Int](tree)(x => x, _ max _)

  def depthViaFold(tree: Tree[_]): Int =
    fold(tree)(_ => 0, (l: Int, r: Int) => l + r + 1)

  def mapViaFold[A,B](tree: Tree[A])(f: A => B): Tree[B] =
    fold(tree)(a => Leaf(f(a)), (l: Tree[B], r: Tree[B]) => Branch(l, r))
}