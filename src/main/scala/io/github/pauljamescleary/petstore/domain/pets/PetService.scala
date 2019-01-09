package io.github.pauljamescleary.petstore.domain.pets

import scala.language.higherKinds
import cats._
import cats.data._
import io.chrisdavenport.log4cats.Logger
import io.github.pauljamescleary.petstore.domain.{PetAlreadyExistsError, PetNotFoundError}

/**
  * The entry point to our domain, works with repositories and validations to implement behavior
  *
  * @param repository where we get our data
  * @param validation something that provides validations to the service
  * @tparam F - this is the container for the things we work with, could be scala.concurrent.Future, Option, anything
  *           as long as it is a Monad
  */
class PetService[F[_]](
    logger: Logger[F],
    repository: PetRepositoryAlgebra[F],
    validation: PetValidationAlgebra[F]) {
  import cats.syntax.all._

  def create(pet: Pet)(implicit M: Monad[F]): EitherT[F, PetAlreadyExistsError, Pet] =
    for {
      _ <- EitherT.liftF(logger.info(s"Creating pet $pet"))
      _ <- validation.doesNotExist(pet)
      saved <- EitherT.liftF(repository.create(pet))
    } yield saved

  /* Could argue that we could make this idempotent on put and not check if the pet exists */
  def update(pet: Pet)(implicit M: Monad[F]): EitherT[F, PetNotFoundError.type, Pet] =
    for {
      _ <- EitherT.liftF(logger.info(s"Updating pet $pet"))
      _ <- validation.exists(pet.id)
      saved <- EitherT.fromOptionF(repository.update(pet), PetNotFoundError)
    } yield saved

  def get(id: Long)(implicit M: Monad[F]): EitherT[F, PetNotFoundError.type, Pet] =
    for {
      _ <- EitherT.liftF(logger.info(s"Fetching pet with id $id"))
      pet <- EitherT.fromOptionF(repository.get(id), PetNotFoundError)
    } yield pet

  /* In some circumstances we may care if we actually delete the pet; here we are idempotent and do not care */
  def delete(id: Long)(implicit M: Monad[F]): F[Unit] =
    for {
      _ <- logger.info(s"Deleting pet with id $id")
      _ <- repository.delete(id).as(())
    } yield ()

  def list(pageSize: Int, offset: Int): F[List[Pet]] =
    for {
      _ <- logger.debug(s"Fetching page with size $pageSize and offset $offset")
      page <- repository.list(pageSize, offset)
    } yield page

  def findByStatus(statuses: NonEmptyList[PetStatus]): F[List[Pet]] =
    for {
      _ <- logger.debug(s"Looking up pets with statuses $statuses")
      pets <- repository.findByStatus(statuses)
    } yield pets

  def findByTag(tags: NonEmptyList[String]): F[List[Pet]] =
    for {
      _ <- logger.debug(s"Requesting pets with tags $tags")
      pets <- repository.findByTag(tags)
    } yield pets

}

object PetService {
  def apply[F[_]: Monad](
      logger: Logger[F],
      repository: PetRepositoryAlgebra[F],
      validation: PetValidationAlgebra[F]) =
    new PetService[F](logger, repository, validation)
}
