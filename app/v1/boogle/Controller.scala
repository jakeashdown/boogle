package v1.boogle

import javax.inject.Inject

import play.api.Logger
import play.api.data.Form
import play.api.libs.json.Json
import play.api.mvc._

import scala.concurrent.{ExecutionContext, Future}

case class BookInput(title: String, author: String, pages: List[String])
case class PageInput(number: Int, content: String)

/**
  * Takes HTTP requests and produces JSON.
  */
class Controller @Inject()(cc: BoogleControllerComponents)(implicit ec: ExecutionContext)
    extends BoogleBaseController(cc) {

  private val logger = Logger(getClass)

  private val bookForm: Form[BookInput] = {
    import play.api.data.Forms._

    Form(
      mapping(
        "title" -> nonEmptyText,
        "author" -> nonEmptyText,
        "pages" -> list(nonEmptyText)
      )(BookInput.apply)(BookInput.unapply)
    )
  }

  private val pageForm: Form[PageInput] = {
    import play.api.data.Forms._

    Form(
      mapping(
        "number" -> number(1),
        "content" -> nonEmptyText
      )(PageInput.apply)(PageInput.unapply)
    )
  }

  def indexBookForSearch: Action[AnyContent] = BoogleActionBuilder.async { implicit request =>
    def processJsonBook[A]()(implicit request: BoogleRequest[A]): Future[Result] = {
      def failure(badForm: Form[BookInput]) = {
        Future.successful(BadRequest(badForm.errorsAsJson))
      }
      def success(input: BookInput) = {
        resourceHandler.indexBookData(input) map(id => Created(id))
      }
      logger.trace(s"indexing book for search: $request")
      bookForm.bindFromRequest().fold(failure, success)
    }
    processJsonBook()
  }

  def indexPageOfBookForSearch(bookId: String): Action[AnyContent] = BoogleActionBuilder.async { implicit request =>
    def processJsonPage[A](bookId: String)(implicit request: BoogleRequest[A]): Future[Result] = {
      def failure(badForm: Form[PageInput]) = {
        Future.successful(BadRequest(badForm.errorsAsJson))
      }
      def success(input: PageInput) = {
        resourceHandler.indexPageData(input, bookId) map { resource => Created(Json.toJson(resource))
        } recover {
          case error: NoSuchBookError => BadRequest(error.toString)
        }

      }
      logger.trace(s"indexing page for search: $request")
      pageForm.bindFromRequest().fold(failure, success)
    }
    processJsonPage(bookId)
  }

  def fastSearchOfPages(searchPhrase: String): Action[AnyContent] = BoogleActionBuilder.async { implicit request =>
    logger.trace(s"fast search of book pages: $request")
    resourceHandler.getPageResourceForSearchString(searchPhrase) map(page =>
      Ok(Json.toJson(page))
    )
  }

  def deIndexBook(bookId: String): Action[AnyContent] = BoogleActionBuilder.async { implicit request =>
    logger.trace(s"de-index book: $request")
    resourceHandler.delete(bookId) map(_ =>
        Ok(Json.toJson("book de-indexed"))
    )
  }

}
