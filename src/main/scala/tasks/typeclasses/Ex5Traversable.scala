package u04lab
import u03.Sequences.* 
import Sequence.*
import u03.Optionals.*

/*  Exercise 5: 
 *  - Generalise by ad-hoc polymorphism logAll, such that:
 *  -- it can be called on Sequences but also on Optional, or others... 
 *  -- it does not necessarily call log, but any function with analogous type
 *  - Hint: introduce a type class Traversable[T[_]]], capturing the ability of calling a
 *    "consumer function" on all elements (with type A) of a datastructure T[A] 
 *    Note Traversable is a 2-kinded trait (similar to Filterable, or Monad)
 *  - Write givens for Traversable[Optional] and Traversable[Sequence]
 *  - Show you can use the generalisation of logAll to:
 *  -- log all elements of an Optional, or of a Traversable
 *  -- println(_) all elements of an Optional, or of a Traversable
 */

object Ex5Traversable:

  trait Traversable[T[_]]:
    def traversate[A](t: T[A])(f: A => Unit): Unit

  given Traversable[Optional] with
    def traversate[A](t: Optional[A])(f: A => Unit): Unit = t match
      case Optional.Just(t) => f(t)
      case Optional.Empty() => ()

  given Traversable[Sequence] with
    def traversate[A](t: Sequence[A])(f: A => Unit): Unit = t match
      case Sequence.Cons(h, tail) => f(h); traversate(tail)(f)
      case Sequence.Nil() => ()

  def log[A](a: A): Unit = println("The next element is: "+a)

  def logAll[T[_]: Traversable, A](t: T[A])(f: A => Unit): Unit =
    val traversable = summon[Traversable[T]]
    traversable.traversate(t)(f(_))

  @main def main =
    val sequence = Sequence.Cons(10, Sequence.Cons(20, Sequence.Nil()))
    val optionalNotEmpty = Optional.Just(5)
    logAll(sequence)(log)
    logAll(optionalNotEmpty)(log)
    logAll(Optional.Empty())
    logAll(sequence)(println)
    logAll(optionalNotEmpty)(println)
    logAll(Optional.Empty())(println)