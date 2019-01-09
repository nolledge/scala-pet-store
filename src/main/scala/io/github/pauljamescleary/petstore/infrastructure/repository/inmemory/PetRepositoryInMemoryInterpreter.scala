package io.github.pauljamescleary.petstore.infrastructure.repository.inmemory

import scala.collection.concurrent.TrieMap
import scala.util.Random
import cats._
import cats.data.NonEmptyList
import cats.implicits._
import io.chrisdavenport.log4cats.Logger
import io.github.pauljamescleary.petstore.domain.pets.{Pet, PetRepositoryAlgebra, PetStatus}

class PetRepositoryInMemoryInterpreter[F[_]: Applicative](logger: Logger[F])
    extends PetRepositoryAlgebra[F] {

  private val cache = new TrieMap[Long, Pet]

  private val random = new Random

  def create(pet: Pet): F[Pet] = {
    val id = random.nextLong
    val toSave = pet.copy(id = id.some)
    cache += (id -> pet.copy(id = id.some))
    toSave.pure[F]
  }

  def update(pet: Pet): F[Option[Pet]] = pet.id.traverse { id =>
    logger
      .info(s"Updating pet $pet")
      .map { _ =>
        cache.update(id, pet)
        pet
      }
  }

  def get(id: Long): F[Option[Pet]] =
    logger.debug(s"Getting pet with id $id").map(_ => cache.get(id))

  def delete(id: Long): F[Option[Pet]] =
    logger.info(s"Deleting pet with $id").map(_ => cache.remove(id))

  def findByNameAndCategory(name: String, category: String): F[Set[Pet]] =
    logger
      .debug(s"Finding pet with name $name and category $category")
      .map(
        _ =>
          cache.values
            .filter(p => p.name == name && p.category == category)
            .toSet)

  def list(pageSize: Int, offset: Int): F[List[Pet]] =
    logger
      .debug(s"Listing pets - pageSize: $pageSize and offset $offset")
      .map(_ => cache.values.toList.sortBy(_.name).slice(offset, offset + pageSize))

  def findByStatus(statuses: NonEmptyList[PetStatus]): F[List[Pet]] =
    logger
      .debug(s"Finding pets with statuses $statuses")
      .map(_ => cache.values.filter(p => statuses.exists(_ == p.status)).toList)

  def findByTag(tags: NonEmptyList[String]): F[List[Pet]] =
    logger
      .debug(s"Finding pets with tags $tags")
      .map(_ => cache.values.filter(p => tags.exists(_ == p.tags)).toList)
}

object PetRepositoryInMemoryInterpreter {
  def apply[F[_]: Applicative](logger: Logger[F]) = new PetRepositoryInMemoryInterpreter[F](logger)
}
