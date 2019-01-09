package io.github.pauljamescleary.petstore.domain.users

import cats._
import cats.data._
import cats.syntax.functor._
import io.chrisdavenport.log4cats.Logger
import io.github.pauljamescleary.petstore.domain.{UserAlreadyExistsError, UserNotFoundError}

class UserService[F[_]: Monad](
    logger: Logger[F],
    userRepo: UserRepositoryAlgebra[F],
    validation: UserValidationAlgebra[F]) {

  def createUser(user: User): EitherT[F, UserAlreadyExistsError, User] =
    for {
      _ <- logger.info(s"Attempting to create user $user")
      _ <- validation.doesNotExist(user)
      saved <- EitherT.liftF(userRepo.create(user))
    } yield saved

  def getUser(userId: Long): EitherT[F, UserNotFoundError.type, User] = for {
    _ <- EitherT.liftF(logger.info(s"Fetching user with Id $userId"))
    user <- EitherT.fromOptionF(userRepo.get(userId), UserNotFoundError)
  } yield user

  def getUserByName(userName: String): EitherT[F, UserNotFoundError.type, User] = for {
    _ <- EitherT.liftF(logger.info(s"Fetching user by name $userName"))
    user <- EitherT.fromOptionF(userRepo.findByUserName(userName), UserNotFoundError)
  } yield user

  def deleteUser(userId: Long): F[Unit] = for {
    _ <- logger.info(s"Deleting user with id $userId")
    _ <- userRepo.delete(userId).as(())
  }  yield ()

  def deleteByUserName(userName: String): F[Unit] = for {
    _ <- logger.info(s"Deleting user by name $userName")
    _ <- userRepo.deleteByUserName(userName).as(())
    } yield ()

  def update(user: User): EitherT[F, UserNotFoundError.type, User] =
    for {
      _ <- EitherT.liftF(logger.info(s"Updating user $user"))
      _ <- validation.exists(user.id)
      saved <- EitherT.fromOptionF(userRepo.update(user), UserNotFoundError)
    } yield saved

  def list(pageSize: Int, offset: Int): F[List[User]] =
    userRepo.list(pageSize, offset)
}

object UserService {
  def apply[F[_]: Monad](
      repository: UserRepositoryAlgebra[F],
      validation: UserValidationAlgebra[F],
      logger: Logger[F]): UserService[F] =
    new UserService[F](logger, repository, validation)
}
