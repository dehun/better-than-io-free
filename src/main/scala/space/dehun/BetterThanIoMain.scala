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

  def foo(x:Int) = for {
    user <- Db.queryUser(x.toString).inject[AppStack]
    _ <- Log.logMsg(s"got user ${user}").inject[AppStack]
    _ <- Db.storeUser(user.copy(age=user.age + 1)).inject[AppStack]
  } yield user

  def bar(user:User) = for {
    _ <- Log.logMsg("lets notify user change!").inject[AppStack]
    _ <- Net.notifyUserChange(user).inject[AppStack]
  } yield user.age

  override def main(args: Array[String]): Unit = {
    val logIoNat = new LogIoNat()
    val dbIoNat = new DbIoNat()
    val netIoNat = new NetIoNat()

    val level0Nat = logIoNat.or(netIoNat)
    val appStackNat = dbIoNat.or[Level0](level0Nat)

    val r = (foo(12) flatMap bar)
      .foldMap(appStackNat)
      .unsafeRunSync()
    Console.println(s"end result: $r")
  }
}
