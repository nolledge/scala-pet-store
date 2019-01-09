package io.github.pauljamescleary.petstore.infrastructure.repository.inmemory

import scala.collection.concurrent.TrieMap
import scala.util.Random
import cats._
import cats.implicits._
import io.chrisdavenport.log4cats.Logger
import io.github.pauljamescleary.petstore.domain.orders.{Order, OrderRepositoryAlgebra}

class OrderRepositoryInMemoryInterpreter[F[_]: Applicative](logger: Logger[F])
    extends OrderRepositoryAlgebra[F] {

  private val cache = new TrieMap[Long, Order]

  private val random = new Random

  def create(order: Order): F[Order] = {
    val toSave = order.copy(id = order.id.orElse(random.nextLong.some))
    logger.info(s"Creating order $order").map { _ =>
      toSave.id.foreach { cache.put(_, toSave) }
      toSave
    }
  }

  def get(orderId: Long): F[Option[Order]] =
    logger.debug(s"Getting order with id $orderId")
      .map(_ => cache.get(orderId))

  def delete(orderId: Long): F[Option[Order]] =
    logger.info(s"Deleting order with id $orderId")
      .map(_ => cache.remove(orderId))
}

object OrderRepositoryInMemoryInterpreter {
  def apply[F[_]: Applicative](logger: Logger[F]) = new OrderRepositoryInMemoryInterpreter[F](logger)
}
