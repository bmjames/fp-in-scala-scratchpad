
object Main extends Application {

  import TreeExercises._

  assert(size(Leaf(1)) == 1)
  assert(size(Branch(Leaf(1), Leaf(2))) == 2)
  assert(size(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) == 3)

  assert(maximum(Leaf(1)) == 1)
  assert(maximum(Branch(Leaf(1), Leaf(2))) == 2)
  assert(maximum(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) == 3)

  assert(depth(Leaf(1)) == 0)
  assert(depth(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) == 2)
  assert(depth(Branch(Leaf(1), Branch(Branch(Leaf(2), Leaf(3)), Leaf(4)))) == 3)

  assert(map(Leaf(1), (_: Int) * 2) == Leaf(2))
  assert(map(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))), (_: Int) * 2) == Branch(Leaf(2), Branch(Leaf(4), Leaf(6))))

  assert(size2(Leaf(1)) == 1)
  assert(size2(Branch(Leaf(1), Leaf(2))) == 2)
  assert(size2(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) == 3)

  assert(maximum2(Leaf(1)) == 1)
  assert(maximum2(Branch(Leaf(1), Leaf(2))) == 2)
  assert(maximum2(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) == 3)

  assert(depth2(Leaf(1)) == 0)
  assert(depth2(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) == 2)
  assert(depth2(Branch(Leaf(1), Branch(Branch(Leaf(2), Leaf(3)), Leaf(4)))) == 3)

  assert(map2(Leaf(1), (_: Int) * 2) == Leaf(2))
  assert(map2(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))), (_: Int) * 2) == Branch(Leaf(2), Branch(Leaf(4), Leaf(6))))

  assert(id(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) == Branch(Leaf(1), Branch(Leaf(2), Leaf(3))))
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeExercises {

  def size(tree: Tree[_]): Int =
    tree match {
      case Leaf(_) => 1
      case Branch(left, right) => size(left) + size(right)
    }

  def maximum(tree: Tree[Int]): Int =
    tree match {
      case Leaf(x) => x
      case Branch(left, right) => math.max(maximum(left), maximum(right))
    }

  def depth(tree: Tree[_]): Int =
    tree match {
      case Leaf(_) => 0
      case Branch(left, right) => 1 + math.max(depth(left), depth(right))
    }

  def map[A, B](tree: Tree[A], f: A => B): Tree[B] =
    tree match {
      case Leaf(a) => Leaf(f(a))
      case Branch(left, right) => Branch(map(left, f), map(right, f))
    }

  def fold[A, B](l: A => B, b: (B, B) => B)(tree: Tree[A]): B =
    tree match {
      case Leaf(a) => l(a)
      case Branch(left, right) => b(fold(l, b)(left), fold(l, b)(right))
    }

  def size2(tree: Tree[_]): Int =
    fold[Any, Int](_ => 1, _ + _)(tree)

  def maximum2(tree: Tree[Int]): Int =
    fold[Int, Int](identity, math.max)(tree)

  def depth2(tree: Tree[_]): Int =
    fold[Any, Int](_ => 0, (l, r) => 1 + math.max(l, r))(tree)

  def map2[A, B](tree: Tree[A], f: A => B) =
    fold[A, Tree[B]](f andThen Leaf.apply, Branch.apply)(tree)

  def id[A](tree: Tree[A]): Tree[A] =
    fold[A, Tree[A]](Leaf.apply, Branch.apply)(tree)
}
