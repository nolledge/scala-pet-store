package io.github.pauljamescleary.petstore.infrastructure.repository.doobie

import cats._
import cats.data.OptionT
import cats.implicits._
import doobie._
import doobie.implicits._
import io.github.pauljamescleary.petstore.domain.users.{User, UserRepositoryAlgebra}
import SQLPagination._
import io.chrisdavenport.log4cats.Logger

private object UserSQL {
  def insert(user: User)(implicit lh: LogHandler): Update0 = sql"""
    INSERT INTO USERS (USER_NAME, FIRST_NAME, LAST_NAME, EMAIL, HASH, PHONE)
    VALUES (${user.userName}, ${user.firstName}, ${user.lastName}, ${user.email}, ${user.hash}, ${user.phone})
  """.update

  def update(user: User, id: Long)(implicit lh: LogHandler): Update0 = sql"""
    UPDATE USERS
    SET FIRST_NAME = ${user.firstName}, LAST_NAME = ${user.lastName}, EMAIL = ${user.email}, HASH = ${user.hash}, PHONE = ${user.phone}
    WHERE ID = $id
  """.update

  def select(userId: Long)(implicit lh: LogHandler): Query0[User] = sql"""
    SELECT USER_NAME, FIRST_NAME, LAST_NAME, EMAIL, HASH, PHONE, ID
    FROM USERS
    WHERE ID = $userId
  """.query

  def byUserName(userName: String)(implicit lh: LogHandler): Query0[User] = sql"""
    SELECT USER_NAME, FIRST_NAME, LAST_NAME, EMAIL, HASH, PHONE, ID
    FROM USERS
    WHERE USER_NAME = $userName
  """.query[User]

  def delete(userId: Long)(implicit lh: LogHandler): Update0 = sql"""
    DELETE FROM USERS WHERE ID = $userId
  """.update

  def selectAll(implicit lh: LogHandler): Query0[User] = sql"""
    SELECT USER_NAME, FIRST_NAME, LAST_NAME, EMAIL, HASH, PHONE, ID
    FROM USERS
  """.query
}

class DoobieUserRepositoryInterpreter[F[_]: Monad](logger: Logger[F], val xa: Transactor[F])
  extends UserRepositoryAlgebra[F] {

  import UserSQL._

  private implicit val logHandler: LogHandler = Log4CatsLogHandler[F](logger)

  def create(user: User): F[User] =
    insert(user).withUniqueGeneratedKeys[Long]("ID").map(id => user.copy(id = id.some)).transact(xa)

  def update(user: User): F[Option[User]] = OptionT.fromOption[F](user.id).semiflatMap { id =>
    UserSQL.update(user, id).run.transact(xa).as(user)
  }.value

  def get(userId: Long): F[Option[User]] = select(userId).option.transact(xa)

  def findByUserName(userName: String): F[Option[User]] = byUserName(userName).option.transact(xa)

  def delete(userId: Long): F[Option[User]] = OptionT(get(userId)).semiflatMap(user =>
    UserSQL.delete(userId).run.transact(xa).as(user)
  ).value

  def deleteByUserName(userName: String): F[Option[User]] =
    OptionT(findByUserName(userName)).mapFilter(_.id).flatMapF(delete).value

  def list(pageSize: Int, offset: Int): F[List[User]] =
    paginate(pageSize, offset)(selectAll).to[List].transact(xa)
}

object DoobieUserRepositoryInterpreter {
  def apply[F[_]: Monad](logger: Logger[F], xa: Transactor[F]): DoobieUserRepositoryInterpreter[F] =
    new DoobieUserRepositoryInterpreter(logger, xa)
}

