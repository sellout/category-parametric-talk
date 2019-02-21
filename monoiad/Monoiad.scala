import cats.{Functor}
import cats.implicits._
import scala.{Function1}
import spire.math.{Natural}

package object monoiad {

def combineAll(xs: List[Natural]): Natural

def combine(a: Natural, b: Natural): Natural

val default: Natural

def combineAll = {
  case Nil => default
  case x :: Nil => x
  case x :: y :: xs => combineAll(combine(x, y) :: xs)
}

def combineAll = {
  case Nil => default
  case x :: xs => combine(x, combineAll(xs))
}

trait Monoid[A] {
  def product(x: A, y: A): A
  def unit: A

  def associativity(x: A, y: A, z: A): Boolean =
    product(product(x, y), z) == product(x, product(y, z))
  def leftIdentity(x: A): Boolean =
    product(unit, x) == x
  def rightIdentity(x: A): Boolean =
    product(x, unit) == x
}

def fold[A](as: List[A])(implicit A: Monoid[A]) =
  as.foldRight(A.product(_,_))(A.unit)

implicit val conjunctionMonoid =
  new cats.Monoid[Boolean] {
    def empty = true
    def combine(a: Boolean, b: Boolean) = a && b
  }

implicit val disjunctionMonoid =
  new cats.Monoid[Boolean] {
    def empty = false
    def combine(a: Boolean, b: Boolean) = a || b
  }

implicit val additiveMonoid =
  new cats.Monoid[Int] {
    def empty = 0
    def combine(a: Int, b: Int) = a + b
  }

implicit val multiplicativeMonoid =
  new cats.Monoid[Int] {
    def empty = 1
    def combine(a: Int, b: Int) = a * b
  }

implicit val joinMonoid =
  new cats.Monoid[Int] {
    def empty = Int.MinValue
    def combine(a: Int, b: Int) = a max b
  }

implicit val meetMonoid =
  new cats.Monoid[Int] {
    def empty = Int.MaxValue
    def combine(a: Int, b: Int) = a min b
  }

implicit val concatenationMonoid =
  new cats.Monoid[String] {
    def empty = ""
    def combine(a: String, b: String) = a ++ b
  }

type UnbalancedParens = (Int, Int)

implicit val balancingMonoid =
  new cats.Monoid[UnbalancedParens] {
    def empty = (0, 0)
    def combine(a: UnbalancedParens, b: UnbalancedParens) =
      if (a._2 <= b._1)
        (a._1 + (b._1 - a._2), b._2)
      else
        (a._1, (a._2 - b._1) + b._2)
  }

Iso[String, List[Char]](_.toList)(_.mkString(""))

trait TMonoid {
  type Product[_, _]
  type Identity

  def associativity[A, B, C]
      : Product[Product[A, B], C] => Product[A, Product[B, C]]
  def leftIdentity[A]: Product[Identity, A] => A
  def rightIdentity[A]: Product[A, Identity] => A
}

final class Cartesian extends TMonoid {
  type Product[A, B] = (A, B)
  type Identity = Unit

  def associativity[A, B, C]: ((A, B), C) => (A, (B, C)) = ???
  def leftIdentity[A]: (Unit, A) => A = _._2
  def rightIdentity[A]: (A, Unit) => A = _._1
}

final class Cocartesian extends TMonoid {
  type Product[A, B] = Either[A, B]
  type Identity = Nothing

  def associativity[A, B, C]
      : Either[Either[A, B], C] => Either[A, Either[B, C]] = ???
  def leftIdentity[A]: Either[Nothing, A] => A = ???
  def rightIdentity[A]: Either[A, Nothing] => A = ???
}

object types {

trait Monoid[T <: TMonoid, A] {
  def product: T#Product[A, A] => A
  def unit: T#Identity => A
}

implicit val conjunction = new Monoid[Cartesian, Boolean] {
  def product: (Boolean, Boolean) => Boolean = p => p._1 && p._2
  def unit: Unit => Boolean = Function1.const True
}

implicit def cocartesian[A] = new Monoid[Cocartesian, A] {
  def product: Either[A, A] => A = {
    case Left(a) => a
    case Right(a) => a
  }
  def unit: Nothing => A = scala.Predef.identity
}
}

trait Category[⟶[_, _]] {
  def compose[A, B, C](f: B ⟶ C, g: A ⟶ B): A ⟶ C
  def identity[A]: A ⟶ A
}

trait MonoidalCategory {
  type Arrow[_, _]
  type Product[_, _]
  type Identity
}

object category {

trait Monoid[T <: MonoidalCategory, A] {
  def product: T#Arrow[T#Product[A, A], A]
  def unit: T#Arrow[T#Identity, A]
}

implicit val conjunction = new Monoid[Cartesian, Boolean] {
  def product = (p: (Boolean, Boolean)) => p._1 && p._2
  def unit = Function1.const True
}

implicit def cocartesian[A] = new Monoid[Cocartesian, A] {
  def product = {
    case Left(a) => a
    case Right(a) => a
  }
  def unit = scala.Predef.identity
}

final class Op[T <: MonoidalCategory] extends MonoidalCategory {
  type Arrow[A, B] = T#Arrow[B, A]
  type Product[_, _] = T#Product[A, B]
  type Identity = T#Identity
}

implicit def comonoid[A] = new Monoid[Op[Cartesian], A] {
  def product: A => (A, A) = x => (x, x)
  def unit: A => Identity = Function1.const ()
}

trait TMonoidF {
  type Product[_[_], _[_], _]
  type Identity[_]
}

final class Monadic extends TMonoidF {
  type Product[F[_], G[_], A] = cats.data.Nested[F, G, A]
  type Identity[A] = cats.data.Id[A]
}

// Nested[Id, F, ?] =:= F =:= Nested[F, Id, ?]
//         Id[F[_]] =:= F =:= F[Id[_]]

trait MonoidalCategoryF {
  type Arrow[_[_], _[_]]
  type Product[_[_], _[_], _]
  type Identity[_]
}

trait MonoidF[C <: MonoidalCategoryF, F[_]] {
  def product: C#Arrow[C#Product[F, F, ?], F]
  def unit: C#Arrow[C#Identity, F]
}

type Monad[M[_]] = MonoidF[Monadic, M]
// def product: M[M[A]] => M[A] // join
// def unit: Id[A] => M[A]      // pure

def flatMap[M[_]: Functor: Monad, A, B](ma: M[A])(f: A => M[B]) =
  ma.map(f).product

def fold[F[_]: Foldable, A: Monoid](fa: F[A]) =
  fa.foldRight(empty, combine)

implicit def freeMonoid[A] =
  new Monoid[Cartesian, List[A]] {
    def identity = Function1.const Nil: Unit => List[A]
    def op = _1 ++ _2 : (List[A], List[A]) => List[A]
  }

def firstExcept[A: cats.Eq](exception: A) = new Monoid[Cartesian, A] {
  def op = (a, b) => if (a === exception) then b else a
  def identity = exception
}

def lastExcept[A: cats.Eq](exception: A) = new Monoid[Cartesian, A] {
  def op = (a, b) => if (b === exception) then a else b
  def identity = exception
}

trait TRig {
  type Add[_, _]
  type Zero
  type Multiply[_, _]
  type One

  final class Additive extends TMonoid {
    type Product[A, B] = Add[A, B]
    type Identity = Zero
  }
  final class Multiplicative extends TMonoid {
    type Product[A, B] = Multiply[A, B]
    type Identity = One
  }
}

trait RigCategory extends TRig {
  type Arrow[_, _]
}

final class SetCategory extends RigCategory {
  type Arrow[A, B] = Function1[A, B]
  type Add[A, B] = Either[A, B]
  type Zero = Nothing
  type Multiply[A, B] = (A, B)
  type One = Unit
}

// Either[(Bool, Int), (Bool, String)] =:= (Bool, EIther[Int, String])
// (Int, Nothing) =:= Nothing =:= (Nothing, Int)

trait BoundedLattice[C <: MonoidalCategory, L] {
  def meet: C#Arrow[C#Product[L, L], L]
  def minimum: C#Arrow[C#Identity, L]
  def join: C#Arrow[C#Product[L, L], L]
  def maximum: C#Arrow[C#Identity, L]

  final class Meet extends TMonoid {
    def op = meet
    def identity = maximum
  }
  final class Join extends TMonoid {
    def op = join
    def identity = minimum
  }
}

final class Day[F[_], G[_], A] {
  type B
  type C

  def fb: F[B]
  def gc: G[C]
  def fn: (B, C) => A
}

final class Daily extends TMonoidF {
  type Identity[A] = cats.Id[A]
  type Product[F[_], G[_], A] = Day[F, G, A]
}

// Day[Id, F, ?] =:= F =:= Day[F, Id, ?]

// final class Categorical extends TMonoidP {
//   type Identity[A, B] = Is[A, B]
//   type Product[F[_, _], G[_, _], A, B] = Profunctory[F, G, A, B]
// }

trait Category[⟶[_, _]] {
  def compose[A, B, C](f: B ⟶ C, g: A ⟶ B): A ⟶ C
  def identity[A]: A ⟶ A
}

}}
