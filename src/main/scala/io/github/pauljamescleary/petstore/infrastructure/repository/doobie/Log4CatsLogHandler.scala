package io.github.pauljamescleary.petstore.infrastructure.repository.doobie
import cats.effect.{Async, Effect, Sync}
import doobie.util.log
import doobie.util.log.{ExecFailure, LogHandler, ProcessingFailure, Success}
import io.chrisdavenport.log4cats.Logger
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger

class Log4CatsLogHandler[F[_]: Sync](logger: Logger[F]){



  val handler: F[LogHandler] = LogHandler {

      case Success(s, a, e1, e2) =>
        Async[F].
        logger.info(s"""Successful Statement Execution:
            |
            |  ${s.lines.dropWhile(_.trim.isEmpty).mkString("\n  ")}
            |
            | arguments = [${a.mkString(", ")}]
            |   elapsed = ${e1.toMillis} ms exec + ${e2.toMillis} ms processing (${(e1 + e2).toMillis} ms total)
          """.stripMargin)


      case ProcessingFailure(s, a, e1, e2, t) =>
        logger.error(s"""Failed Resultset Processing:
            |
            |  ${s.lines.dropWhile(_.trim.isEmpty).mkString("\n  ")}
            |
            | arguments = [${a.mkString(", ")}]
            |   elapsed = ${e1.toMillis} ms exec + ${e2.toMillis} ms processing (failed) (${(e1 + e2).toMillis} ms total)
            |   failure = ${t.getMessage}
          """.stripMargin)

      case ExecFailure(s, a, e1, t) =>
        logger.error(s"""Failed Statement Execution:
            |
            |  ${s.lines.dropWhile(_.trim.isEmpty).mkString("\n  ")}
            |
            | arguments = [${a.mkString(", ")}]
            |   elapsed = ${e1.toMillis} ms exec (failed)
            |   failure = ${t.getMessage}
          """.stripMargin)

    }
  }

}

object Log4CatsLogHandler {

  def apply[F[_]](logger: Logger[F]): LogHandler = new Log4CatsLogHandler[F](logger).handler
}
