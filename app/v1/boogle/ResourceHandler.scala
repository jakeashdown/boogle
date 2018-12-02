package v1.boogle

import javax.inject.{Inject, Provider}

import play.api.MarkerContext

import scala.concurrent.{ExecutionContext, Future}
import play.api.libs.json._

/**
  * DTOs
  */
case class BookResource(id: String, title: String, author: String, pages: Map[Int, String])
case class PageResource(id: String, bookId: String, bookTitle: String, number: String, content: String)
case class DeleteResource(success: Boolean)

object BookResource {
  /**
    * Mapping to write a BookResource out as a JSON value.
    */
  implicit val implicitWrites = new Writes[BookResource] {
    def writes(resource: BookResource): JsValue = {
      val pages = if (resource.pages != null && resource.pages.size > 0) resource.pages else Map()
      Json.obj(
        "id" -> resource.id,
        "title" -> resource.title,
        "author" -> resource.author,
        "pages" -> pages
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

  def create(bookInput: BookInput)(implicit mc: MarkerContext): Future[BookResource] = {
    // TODO: Fix this mutable mess
    val pageMap = createPageMap(bookInput.pages)
    var data = BookData(null, bookInput.title, bookInput.author, pageMap)
    repository.indexBook(data).map { id =>
      data = BookData(id, bookInput.title, bookInput.author, pageMap)
      createBookResource(data)
    }
  }

  def lookup(searchPhrase: String)(implicit mc: MarkerContext): Future[Option[PageResource]] = {
    val future = repository.getPageBySearchPhrase(searchPhrase)
    future.map { maybeData =>
      maybeData.map { data =>
        createPageResource(data)
      }
    }
  }

  def delete(bookId: String)(implicit mc: MarkerContext): Future[DeleteResource] = {
    repository.deleteBook(bookId) map ( DeleteResource(_) )
  }

  private def createBookResource(data: BookData): BookResource = {
    BookResource(data.id, data.title, data.author, data.pages)
  }

  private def createPageResource(data: PageData): PageResource = {
    PageResource(data.id, data.bookId, data.bookTitle, data.number, data.content)
  }

  // TODO: don't need this, just keep pages as an array
  private def createPageMap(pages: List[String]): Map[Int, String] = {
    var pageMap: Map[Int, String] = Map()
    (0 to (pages.size - 1)).foreach(i => pageMap += (i -> pages(i)))
    pageMap
  }

}
