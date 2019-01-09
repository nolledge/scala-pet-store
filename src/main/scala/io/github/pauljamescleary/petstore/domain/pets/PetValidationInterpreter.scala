package io.github.pauljamescleary.petstore.domain.pets

import cats._
import cats.data.EitherT
import cats.implicits._
import io.chrisdavenport.log4cats.Logger
import io.github.pauljamescleary.petstore.domain.{PetAlreadyExistsError, PetNotFoundError}

class PetValidationInterpreter[F[_]: Monad](logger: Logger[F], repository: PetRepositoryAlgebra[F])
    extends PetValidationAlgebra[F] {

  def doesNotExist(pet: Pet): EitherT[F, PetAlreadyExistsError, Unit] = EitherT {
    repository
      .findByNameAndCategory(pet.name, pet.category)
      .flatMap[Either[PetAlreadyExistsError, Unit]] { matches =>
        if (matches.forall(possibleMatch => possibleMatch.bio != pet.bio)) {
          logger
            .debug(s"Pet $pet does not exist")
            .map(_.asRight)
        } else {
          logger
            .error(s"Pet $pet already exists!")
            .map(_ => PetAlreadyExistsError(pet).asLeft)
        }
      }
  }

  def exists(petId: Option[Long]): EitherT[F, PetNotFoundError.type, Unit] =
    EitherT {
      petId match {
        case Some(id) =>
          // Ensure is a little tough to follow, it says "make sure this condition is true, otherwise throw the error specified
          // In this example, we make sure that the option returned has a value, otherwise the pet was not found
          repository.get(id).flatMap {
            case Some(_) =>
              logger
                .debug(s"pet $petId exists")
                .map(_.asRight)
            case _ =>
              logger
                .error(s"Pet with id $petId not found")
                .map(_ => PetNotFoundError.asLeft)
          }
        case _ =>
          Either.left[PetNotFoundError.type, Unit](PetNotFoundError).pure[F]
      }
    }
}

object PetValidationInterpreter {
  def apply[F[_]: Monad](logger: Logger[F], repository: PetRepositoryAlgebra[F]) =
    new PetValidationInterpreter[F](logger, repository)
}
