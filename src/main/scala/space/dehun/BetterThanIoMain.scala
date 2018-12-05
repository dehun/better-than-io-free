package space.dehun

import cats.~>
import cats._
import cats.data.EitherT
import cats.implicits._
import cats.effect.{Effect, IO}
import cats.free.Free
import cats.Inject._
import cats.data._

object BetterThanIoMain extends App {
  type Level0[A] = EitherK[LogAction, NetAction, A]
  type AppStack[A] = EitherK[DbAction, Level0, A]

  type LogAndDb[A] = EitherK[LogAction, DbAction, A]
  def foo(x:Int):Free[LogAndDb, User] = for {
    user <- Db.queryUser(x.toString).inject[LogAndDb]
    _ <- Log.logMsg(s"got user ${user}").inject[LogAndDb]
    _ <- Db.storeUser(user.copy(age=user.age + 1)).inject[LogAndDb]
  } yield user

  type LogAndNet[A] = EitherK[LogAction, NetAction, A]
  def bar(user:User):Free[LogAndNet, Int] = for {
    _ <- Log.logMsg("lets notify user change!").inject[LogAndNet]
    _ <- Net.notifyUserChange(user).inject[LogAndNet]
  } yield user.age

  override def main(args: Array[String]): Unit = {
    lazy val logIoNat = new LogIoNat()
    lazy val dbIoNat = new DbIoNat()
    lazy val netIoNat = new NetIoNat()

    lazy val level0Nat = logIoNat.or(netIoNat)
    lazy val appStackNat = dbIoNat.or[Level0](level0Nat)

    implicit val logAndDbIntoAppStack = new InjectK[LogAndDb, AppStack] {
      override def inj: ~>[LogAndDb, AppStack] = new ~>[LogAndDb, AppStack] {
        override def apply[A](fa: LogAndDb[A]): AppStack[A] = fa match {
          case EitherK(Left(act)) => InjectK[LogAction, AppStack].inj(act)
          case EitherK(Right(act)) => InjectK[DbAction, AppStack].inj(act)
        }
      }

      override def prj: ~>[AppStack, λ[α => Option[LogAndDb[α]]]] = ??? // not used
    }

    val r = (foo(12).inject[AppStack] flatMap (u => bar(u).inject[AppStack]))
      .foldMap(appStackNat)
      .unsafeRunSync()
    Console.println(s"end result: $r")
  }
}
