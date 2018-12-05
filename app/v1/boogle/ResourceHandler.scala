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

  def indexBookData(bookInput: BookInput)(implicit mc: MarkerContext): Future[BookResource] = {
    val data = BookData(null, bookInput.title, bookInput.author, bookInput.pages)
    repository.indexBookData(data) map(id =>
      createBookResource(BookData(id, bookInput.title, bookInput.author, bookInput.pages))
    )
  }

  def getPageResourceForSearchString(searchPhrase: String)(implicit mc: MarkerContext): Future[Option[PageResource]] = {
    val future = repository.getPageDataBySearchPhrase(searchPhrase)
    future map { maybeData =>
      maybeData map(data => createPageResource(data))
    }
  }

  def delete(bookId: String)(implicit mc: MarkerContext): Future[DeleteResource] = {
    repository.deleteBookByBookId(bookId) map(DeleteResource(_) )
  }

  private def createBookResource(data: BookData): BookResource = {
    BookResource(data.id, data.title, data.author, data.pages)
  }

  private def createPageResource(data: PageData): PageResource = {
    PageResource(data.id, data.bookId, data.bookTitle, data.number, data.content)
  }

}
