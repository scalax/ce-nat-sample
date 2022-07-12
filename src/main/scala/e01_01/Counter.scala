package e01_01

import cats._
import cats.implicits._
import cats.effect._
import cats.effect.kernel.MonadCancel

trait CollectFlatMap[F[_], A] {
  def f[T](fun: A => F[T]): F[T]
}

trait Number[F[+_], +A] {
  def execute[ToDataType, Result](context: Context[ToDataType, A, Result, F])(t: ToDataType): Result
}
case class NumberS[F[+_], +A, E](tail: E => Number[F, A], flatMap: CollectFlatMap[F, E]) extends Number[F, A] {
  override def execute[ToDataType, Result](context: Context[ToDataType, A, Result, F])(t: ToDataType): Result =
    context.bindS(t, tail, flatMap)
}
case class NumberT[F[+_], +A](tail: () => Number[F, A], head: A) extends Number[F, A] {
  override def execute[ToDataType, Result](context: Context[ToDataType, A, Result, F])(t: ToDataType): Result = context.bindT(t, tail, head)
}

trait Context[ToDataType, -A, Result, F[+_]] {
  def bindS[X](t: ToDataType, current: X => Number[F, A], flatMap: CollectFlatMap[F, X]): Result
  def bindT(t: ToDataType, current: () => Number[F, A], head: A): Result
}

class CollectContext[F[+_]] {

  def numberT[T](t: T): Number[F, T] = {
    def numbert: Number[F, T] = NumberT(() => numbert, t)
    numbert
  }

  trait NumberFlatMap[U] {
    def flatMap[T](u: U => Number[F, T]): Number[F, T]
  }
  trait NumberMap[U] {
    def map[T](u: U => T): Number[F, T]
  }

  def liftToN[U](a: F[U])(implicit m: FlatMap[F]): Number[F, U] = NumberS(
    (e: U) => numberT(e),
    new CollectFlatMap[F, U] {
      def f[T](fun: U => F[T]): F[T] = a.flatMap(fun)
    }
  )
  def flatMap[U](a: F[U])(implicit m: FlatMap[F]): NumberFlatMap[U] = new NumberFlatMap[U] {
    override def flatMap[T](u: U => Number[F, T]): Number[F, T] = NumberS(
      u,
      new CollectFlatMap[F, U] {
        def f[T](fun: U => F[T]): F[T] = a.flatMap(fun)
      }
    )
  }
  def map[U](a: F[U])(implicit i1: FlatMap[F]): NumberMap[U] = new NumberMap[U] {
    override def map[T](fun: U => T): Number[F, T] = NumberS(
      (e: U) => numberT(fun(e)),
      new CollectFlatMap[F, U] {
        def f[T](fun: U => F[T]): F[T] = a.flatMap(fun)
      }
    )
  }
  def resource_use[U](a: Resource[F, U])(implicit v: MonadCancel[F, Throwable]): NumberFlatMap[U] = new NumberFlatMap[U] {
    override def flatMap[T](u: U => Number[F, T]): Number[F, T] = NumberS(
      u,
      new CollectFlatMap[F, U] {
        def f[T](fun: U => F[T]): F[T] = a.use(fun)
      }
    )
  }

  def runF[Data](number: Number[F, Data])(implicit f: FlatMap[F], a: Applicative[F]): F[Data] = number.execute(new Runner.runner)(())
  def plusF[Data](number: Number[F, Data])(implicit i: FlatMap[F]): NumberFlatMap[Data] = new NumberFlatMap[Data] {
    override def flatMap[T](u: Data => Number[F, T]): Number[F, T] = number.execute(new Plus.plus[T, Data])(u)
  }
  def plusM[Data](number: Number[F, Data])(implicit i: Functor[F]): NumberMap[Data] = new NumberMap[Data] {
    override def map[T](u: Data => T): Number[F, T] = number.execute(new Plus.plusMap[T, Data])(u)
  }

  private object Runner {
    class runner[Data](implicit f: FlatMap[F], a: Applicative[F]) extends Context[Unit, Data, F[Data], F] {
      override def bindS[X](t: Unit, current: X => Number[F, Data], flatMap: CollectFlatMap[F, X]): F[Data] =
        flatMap.f(n => current(n).execute(this)(()))
      override def bindT(t: Unit, current: () => Number[F, Data], head: Data): F[Data] = Applicative[F].pure(head)
    }
  }

  private object Plus {
    class plus[Data, B](implicit i: Functor[F]) extends Context[B => Number[F, Data], B, Number[F, Data], F] {
      override def bindS[X](
        t: B => Number[F, Data],
        current: X => Number[F, B],
        flatMap: CollectFlatMap[F, X]
      ): Number[F, Data] = {
        val io = (x: X) => current(x).execute(this)(t)
        NumberS(io, flatMap)
      }
      override def bindT(t: B => Number[F, Data], current: () => Number[F, B], head: B): Number[F, Data] = t(head)
    }

    class plusMap[Data, B](implicit i: Functor[F]) extends Context[B => Data, B, Number[F, Data], F] {
      override def bindS[X](
        t: B => Data,
        current: X => Number[F, B],
        flatMap: CollectFlatMap[F, X]
      ): Number[F, Data] = {
        val io = (x: X) => current(x).execute(this)(t)
        NumberS(io, flatMap)
      }
      override def bindT(t: B => Data, current: () => Number[F, B], head: B): Number[F, Data] = numberT(t(head))
    }
  }

}
