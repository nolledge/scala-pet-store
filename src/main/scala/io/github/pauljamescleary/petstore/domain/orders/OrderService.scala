package io.github.pauljamescleary.petstore.domain.orders

import cats.Monad
import cats.data.EitherT
import io.chrisdavenport.log4cats.Logger
import io.github.pauljamescleary.petstore.domain.OrderNotFoundError

import scala.language.higherKinds

class OrderService[F[_]: Monad](logger: Logger[F], orderRepo: OrderRepositoryAlgebra[F]) {
  import cats.syntax.all._

  def placeOrder(order: Order): F[Order] =
    for {
      _ <- logger.info(s"Placing order $order")
      order <- orderRepo.create(order)
    } yield order

  def get(id: Long): EitherT[F, OrderNotFoundError.type, Order] =
    for {
      _ <- EitherT.liftF(logger.debug(s"Fetching order with id $id"))
      order <- EitherT.fromOptionF(orderRepo.get(id), OrderNotFoundError)
    } yield order

  def delete(id: Long): F[Unit] =
    for {
      _ <- logger.info(s"Deleting order with id $id")
      order <- orderRepo.delete(id).as(())
    } yield order
}

object OrderService {
  def apply[F[_]: Monad](logger: Logger[F], orderRepo: OrderRepositoryAlgebra[F]): OrderService[F] =
    new OrderService(logger, orderRepo)
}
