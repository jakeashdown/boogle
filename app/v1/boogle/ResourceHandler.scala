package v1.boogle

import javax.inject.{Inject, Provider}
import play.api.MarkerContext

import scala.concurrent.{ExecutionContext, Future}
import play.api.libs.json._

/**
  * DTOs
  */
case class BookResource(id: String, title: String, author: String, pages: List[String])
case class PageResource(id: String, bookId: String, bookTitle: String, number: String, content: String)
case class DeleteResource(success: Boolean)

final case class NoSuchBookError(id: String) extends Throwable

object BookResource {
  /**
    * Mapping to write a BookResource out as a JSON value.
    */
  implicit val implicitWrites = new Writes[BookResource] {
    def writes(resource: BookResource): JsValue = {
      val pageMap = if (resource.pages != null && resource.pages.nonEmpty) {
        resource.pages.zipWithIndex.map {
          case (content, page) => (page + 1) -> content
        }
      } else List()
      Json.obj(
        "id" -> resource.id,
        "title" -> resource.title,
        "author" -> resource.author,
        "pages" -> pageMap
      )
    }
  }
}

object PageResource {
  /**
    * Mapping to write a PageResource out as a JSON value.
    */
  implicit val implicitWrites = new Writes[PageResource] {
    def writes(resource: PageResource): JsValue = {
      Json.obj(
        "id" -> resource.id,
        "bookId" -> resource.bookId,
        "bookTitle" -> resource.bookTitle,
        "number" -> resource.number,
        "content" -> resource.content
      )
    }
  }
}

object DeleteResource {
  /**
    * Mapping to write a DeleteResource out as a JSON value.
    */
  implicit val implicitWrites = new Writes[DeleteResource] {
    override def writes(resource: DeleteResource): JsValue = {
      Json.obj(
        "success" -> resource.success
      )
    }
  }
}

/**
  * Controls access to the backend Book and Page data
  */
class ResourceHandler @Inject()(routerProvider: Provider[Router],
                                repository: Repository)(implicit ec: ExecutionContext) {

  def indexBookData(input: BookInput)(implicit mc: MarkerContext): Future[String] = {
    val book = BookData(null, input.title, input.author)
    repository.indexBookData(book) flatMap { id =>
        val indexPageFutures = for { (content, index) <- input.pages.zipWithIndex } yield {
            repository.indexPageData(PageData(null, id, (index + 1).toString, content))
        }
        Future.sequence(indexPageFutures) map { _ => id }
    }
  }

  def indexPageData(input: PageInput, bookId: String)(implicit mc: MarkerContext): Future[String] = {
    // Check the book exists
    repository.getBookById(bookId) flatMap {
      case None =>
        throw NoSuchBookError(bookId)
      case Some(_) =>
        repository.indexPageData(PageData(null, bookId, input.number.toString, input.content))
    }
  }

  def getPageResourceForQuery(query: String)(implicit mc: MarkerContext): Future[Option[PageResource]] = {
    repository.searchForPageByContent(query) flatMap {
      case None => Future(None)
      case Some(page) =>
        // Get the title of the book containing this page
        repository.getBookById(page.bookId) map {
          case None =>
            throw NoSuchBookError(page.bookId)
          case Some(book) =>
            Option(PageResource(page.id, page.bookId, book.title, page.number, page.content))
        }
    }
  }

  def delete(bookId: String)(implicit mc: MarkerContext): Future[Unit] = {
    repository.getBookById(bookId) flatMap {
      case None =>
        throw NoSuchBookError(bookId)
      case Some(book) =>
        // Delete the pages before deleting the book
        repository.searchForPagesByBookId(bookId) flatMap { pages =>
          val deletePageFutures = pages map { page => repository.deletePageById(page.id) }
          Future.sequence(deletePageFutures)
        } flatMap { _ =>
          repository.deleteBookById(bookId)
        }
    }
  }
}
