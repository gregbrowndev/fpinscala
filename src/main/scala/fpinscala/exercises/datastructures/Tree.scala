package fpinscala.exercises.datastructures

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  def size: Int = this match
    case Leaf(_) => 1
    case Branch(l, r) => 1 + l.size + r.size

  def depth: Int = this match
    case Leaf(_) => 0
    case Branch(l, r) => 1 + l.depth.max(r.depth)

//  def depth: Int =
//    @annotation.tailrec
//    def go(t: Tree[A], acc: Int): Int = t match
//      case Leaf(_) => acc
//      case Branch(l, r) =>
//        val leftDepth = go(l, acc + 1)
//        val rightDepth = go(r, acc + 1)
//        if (leftDepth > rightDepth) leftDepth else rightDepth
//
//    go(this, 0)

  def map[B](f: A => B): Tree[B] = this match
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(l.map(f), r.map(f))

  def fold[B](f: A => B, g: (B,B) => B): B = this match
    case Leaf(v) => f(v)
    case Branch(l, r) => g(l.fold(f, g), r.fold(f, g))

  def sizeViaFold: Int =
    this.fold(_ => 1, (l, r) => 1 + l + r)
  
  def depthViaFold: Int =
    this.fold(_ => 0, (l, r) => 1 + l.max(r))
  
  def mapViaFold[B](f: A => B): Tree[B] =
    this.fold(v => Leaf(f(v)), (l, r) => Branch(l, r))

object Tree:

  def size[A](t: Tree[A]): Int = t match
    case Leaf(_) => 1
    case Branch(l,r) => 1 + size(l) + size(r)

  extension (t: Tree[Int]) def firstPositive: Int = t match
    case Leaf(v) => v
    case Branch(l, r) =>
      val lvalue = l.firstPositive
      if lvalue > 0 then lvalue
      else r.firstPositive

  extension (t: Tree[Int]) def maximum: Int = t match
    case Leaf(v) => v
    case Branch(l, r) => l.maximum.max(r.maximum)

  extension (t: Tree[Int]) def maximumViaFold: Int =
    t.fold(v => v, (l, r) => l.max(r))

/* REPL
import fpinscala.exercises.datastructures.Tree
import Tree.*

val tree = Tree.Branch(Tree.Branch(Tree.Leaf(1), Tree.Leaf(2)), Tree.Leaf(3))

tree.maximum
tree.depth
*/