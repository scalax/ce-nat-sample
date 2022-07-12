package e01_01

import cats.effect._
import cats.implicits._
import sttp.client3.asynchttpclient.cats.AsyncHttpClientCatsBackend
import sttp.client3._
import sttp.model.Uri

object Base {

  object ctx extends CollectContext[IO]
  import ctx._

  private def request(uri: Uri) = for {
    request <- flatMap(IO(basicRequest.get(uri)))
    backend <- resource_use(AsyncHttpClientCatsBackend.resource[IO]())
    result  <- map(request.send(backend))
  } yield result.body.merge

  def requestAction(uri: Uri): IO[String] = runF(request(uri))

}

object Compare {

  def request(uri: Uri): IO[String] = {
    val resource = AsyncHttpClientCatsBackend.resource[IO]()
    for {
      request <- IO(basicRequest.get(uri))
      result  <- resource.use(backend => request.send(backend))
    } yield result.body.merge
  }

}
