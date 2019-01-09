package io.github.pauljamescleary.petstore

import config.{DatabaseConfig, PetStoreConfig}
import domain.users._
import domain.orders._
import domain.pets._
import infrastructure.endpoint.{OrderEndpoints, PetEndpoints, UserEndpoints}
import infrastructure.repository.doobie.{
  DoobieOrderRepositoryInterpreter,
  DoobiePetRepositoryInterpreter,
  DoobieUserRepositoryInterpreter,
  Sl4jLogHandler
}
import cats.effect._
import cats.implicits._
import doobie.util.log.LogHandler
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import org.http4s.server.{Router, Server => H4Server}
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.implicits._
import org.slf4j.LoggerFactory
import tsec.mac.jca.HMACSHA256
import tsec.passwordhashers.jca.BCrypt

import scala.concurrent.ExecutionContext.Implicits.global

object Server extends IOApp {
  private val keyGen = HMACSHA256

  def createServer[F[_]: ContextShift: ConcurrentEffect: Timer]: Resource[F, H4Server[F]] = {
    val sl4jLogger = LoggerFactory.getLogger(this.getClass)
    implicit val lh: LogHandler = Sl4jLogHandler(sl4jLogger)
    for {
      conf <- Resource.liftF(PetStoreConfig.load[F])
      signingKey <- Resource.liftF(keyGen.generateKey[F])
      xa <- DatabaseConfig.dbTransactor(conf.db, global, global)
      logger <- Resource.liftF(Slf4jLogger.fromSlf4j[F](sl4jLogger))
      petRepo = DoobiePetRepositoryInterpreter[F](xa)
      orderRepo = DoobieOrderRepositoryInterpreter[F](xa)
      userRepo = DoobieUserRepositoryInterpreter[F](xa)
      petValidation = PetValidationInterpreter[F](logger, petRepo)
      petService = PetService[F](logger, petRepo, petValidation)
      userValidation = UserValidationInterpreter[F](logger, userRepo)
      orderService = OrderService[F](logger, orderRepo)
      userService = UserService[F](logger, userRepo, userValidation)
      services = PetEndpoints.endpoints[F](petService) <+>
        OrderEndpoints.endpoints[F](orderService) <+>
        UserEndpoints.endpoints[F, BCrypt](userService, BCrypt.syncPasswordHasher[F])
      httpApp = Router("/" -> services).orNotFound
      _ <- Resource.liftF(DatabaseConfig.initializeDb(conf.db))
      server <- BlazeServerBuilder[F]
        .bindHttp(conf.server.port, conf.server.host)
        .withHttpApp(httpApp)
        .resource
    } yield server
  }

  def run(args: List[String]): IO[ExitCode] = createServer.use(_ => IO.never).as(ExitCode.Success)
}
