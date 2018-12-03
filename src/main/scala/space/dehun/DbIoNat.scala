package space.dehun

import cats.effect.IO
import cats.~>

class DbIoNat extends ~>[DbAction, IO] {
  override def apply[A](fa: DbAction[A]): IO[A] = fa match {
    case DbAction.QueryUser(userId) => IO {
      Console.println(s"db: querying user $userId")
      User(userId, "somebody", 32)
    }

    case DbAction.StoreUser(user) => IO {
      Console.println(s"db: storing user $user")
    }
  }
}
