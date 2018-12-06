package v1.boogle

import javax.inject.Inject
import play.api.routing.Router.Routes
import play.api.routing.SimpleRouter
import play.api.routing.sird.{DELETE, _}

/**
  * Routes and URLs to the controller.
  */
class Router @Inject()(controller: Controller) extends SimpleRouter {
  val prefix = "/v1/book"

  override def routes: Routes = {
    case POST(p"/") =>
      controller.indexBookForSearch

    case POST(p"/$bookId/page") =>
      controller.indexPageOfBookForSearch(bookId)

    case GET(p"/page" ? q_?"searchPhrase=$searchPhrase") =>
      controller.fastSearchOfPages(searchPhrase.get)

    case DELETE(p"/$bookId") =>
      controller.deIndexBook(bookId)

  }
}
