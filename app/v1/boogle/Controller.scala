package v1.boogle

import javax.inject.Inject

import play.api.Logger
import play.api.data.Form
import play.api.libs.json.Json
import play.api.mvc._

import scala.concurrent.{ExecutionContext, Future}

case class BookInput(title: String, author: String, pages: List[String])

/**
  * Takes HTTP requests and produces JSON.
  */
class Controller @Inject()(cc: BoogleControllerComponents)(implicit ec: ExecutionContext)
    extends BoogleBaseController(cc) {

  private val logger = Logger(getClass)

  private val form: Form[BookInput] = {
    import play.api.data.Forms._

    Form(
      mapping(
        "title" -> nonEmptyText,
        "author" -> nonEmptyText,
        "pages" -> list(text)
      )(BookInput.apply)(BookInput.unapply)
    )
  }

  def indexBookForSearch: Action[AnyContent] = BoogleActionBuilder.async { implicit request =>
    def processJsonBook[A]()(implicit request: BoogleRequest[A]): Future[Result] = {
      def failure(badForm: Form[BookInput]) = {
        Future.successful(BadRequest(badForm.errorsAsJson))
      }

      def success(input: BookInput) = {
        resourceHandler.indexBookData(input).map { resource =>
          Created(Json.toJson(resource))
        }
      }
      logger.trace(s"indexing book for search: $request")
      form.bindFromRequest().fold(failure, success)
    }
    processJsonBook()
  }

  def fastSearchOfPages(searchPhrase: String): Action[AnyContent] = BoogleActionBuilder.async { implicit request =>
    logger.trace(s"fast search of book pages: $request")
    resourceHandler.getPageResourceForSearchString(searchPhrase).map { page =>
      Ok(Json.toJson(page))
    }
  }

  def deIndexBook(bookId: String): Action[AnyContent] = BoogleActionBuilder.async { implicit request =>
    logger.trace(s"de-index book: $request")
    resourceHandler.delete(bookId) map ( success =>
        Ok(Json.toJson(success))
    )
  }

}
