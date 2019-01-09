package io.github.pauljamescleary.petstore.domain.users

import cats._
import cats.data.EitherT
import cats.implicits._
import io.chrisdavenport.log4cats.Logger
import io.github.pauljamescleary.petstore.domain.{UserAlreadyExistsError, UserNotFoundError}

class UserValidationInterpreter[F[_]: Monad](
    logger: Logger[F],
    userRepo: UserRepositoryAlgebra[F]
) extends UserValidationAlgebra[F] {

  def doesNotExist(user: User) = EitherT {
    userRepo.findByUserName(user.userName).flatMap {
      case None =>
        logger.debug("user does exist.").map(_.asRight)
      case Some(_) =>
        logger.warn("user does not exist!").map(_ => UserAlreadyExistsError(user).asLeft)
    }
  }

  def exists(userId: Option[Long]): EitherT[F, UserNotFoundError.type, Unit] =
    EitherT {
      userId
        .map { id =>
          userRepo.get(id).flatMap[Either[UserNotFoundError.type, Unit]] {
            case Some(_) =>
              logger.debug(s"User $id exists").map(_.asRight)
            case _ =>
              logger
                .error(s"User with id $id does not exist")
                .map(_ => UserNotFoundError.asLeft)
          }
        }
        .getOrElse(
          Either.left[UserNotFoundError.type, Unit](UserNotFoundError).pure[F]
        )
    }
}

object UserValidationInterpreter {
  def apply[F[_]: Monad](
      logger: Logger[F],
      repo: UserRepositoryAlgebra[F]
  ): UserValidationAlgebra[F] =
    new UserValidationInterpreter[F](logger, repo)
}
