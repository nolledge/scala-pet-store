package io.github.pauljamescleary.petstore.infrastructure.repository.inmemory

import java.util.Random

import cats.implicits._
import cats.Applicative
import io.chrisdavenport.log4cats.Logger
import io.github.pauljamescleary.petstore.domain.users.{User, UserRepositoryAlgebra}

import scala.collection.concurrent.TrieMap

class UserRepositoryInMemoryInterpreter[F[_]: Applicative](logger: Logger[F])
    extends UserRepositoryAlgebra[F] {

  private val cache = new TrieMap[Long, User]

  private val random = new Random

  def create(user: User): F[User] = logger.info(s"Creating user $user").map { _ =>
    val id = random.nextLong
    val toSave = user.copy(id = id.some)
    cache += (id -> toSave)
    toSave
  }

  def update(user: User): F[Option[User]] = user.id.traverse { id =>
    logger.info(s"Updating user $user").map { _ =>
      cache.update(id, user)
      user
    }
  }

  def get(id: Long): F[Option[User]] =
    logger.debug(s"getting user with id $id").map(_ => cache.get(id))

  def delete(id: Long): F[Option[User]] =
    logger
      .info(s"deleting user with id $id")
      .map(_ => cache.remove(id))

  def findByUserName(userName: String): F[Option[User]] =
    logger
      .debug(s"finding user with name $userName")
      .map(_ => cache.values.find(u => u.userName == userName))

  def list(pageSize: Int, offset: Int): F[List[User]] =
    logger
      .debug(s"Listing pets - pageSize: $pageSize and offset $offset")
      .map(_ => cache.values.toList.sortBy(_.lastName).slice(offset, offset + pageSize))

  def deleteByUserName(userName: String): F[Option[User]] =
    logger
      .info(s"Deleting user by name $userName")
      .map(_ =>
        for {
          user <- cache.values.find(u => u.userName == userName)
          removed <- cache.remove(user.id.get)
        } yield removed)
}

object UserRepositoryInMemoryInterpreter {
  def apply[F[_]: Applicative](logger: Logger[F]) = new UserRepositoryInMemoryInterpreter[F](logger)
}
