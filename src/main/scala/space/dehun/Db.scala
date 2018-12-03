package space.dehun
import cats._
import cats.free.Free
import cats.implicits._

case class User(userId:String, nickName:String, age:Int)

sealed trait DbAction[A]
object DbAction {
  case class QueryUser(userId:String) extends DbAction[User]
  case class StoreUser(newUser:User) extends DbAction[Unit]
}

object Db {
  def queryUser(userId:String) = Free.liftF(DbAction.QueryUser(userId))
  def storeUser(newUser:User) = Free.liftF(DbAction.StoreUser(newUser))
}
