package v1.boogle

import javax.inject.{Inject, Singleton}
import akka.actor.ActorSystem
import com.sksamuel.elastic4s.http.{ElasticClient, ElasticProperties, RequestFailure}
import com.sksamuel.elastic4s.http.delete.DeleteResponse
import com.sksamuel.elastic4s.http.get.GetResponse
import com.sksamuel.elastic4s.http.index.IndexResponse
import play.api.libs.concurrent.CustomExecutionContext
import play.api.{Logger, MarkerContext}

import scala.concurrent.Future

final case class BookData(id: String, title: String, author: String)
final case class PageData(id: String, bookId: String, number: String, content: String)

final case class IndexBookError(failure: RequestFailure) extends Throwable
final case class IndexPageError(failure: RequestFailure) extends Throwable
final case class GetBookError(failure: RequestFailure) extends Throwable
final case class SearchPageError(failure: RequestFailure) extends Throwable
final case class DeletePageError(failure: RequestFailure) extends Throwable

class BoogleExecutionContext @Inject()(actorSystem: ActorSystem) extends CustomExecutionContext(actorSystem, "repository.dispatcher")

/**
  * A pure non-blocking interface for the Repository.
  */
trait Repository {
  // Return the ID of the indexed book or page
  def indexBookData(data: BookData)(implicit mc: MarkerContext): Future[String]
  def indexPageData(data: PageData)(implicit mc: MarkerContext): Future[String]

  // Get all books
  def getBooks()(implicit mc: MarkerContext): Future[List[BookData]]

  // Gets the book by ID, if it exists
  def getBookById(bookId: String)(implicit mc: MarkerContext): Future[Option[BookData]]

  // Return the page matching the search phrase, if it exists
  def searchForPageByContent(content: String)(implicit mc: MarkerContext): Future[Option[PageData]]

  // Return all pages associated with a book, in order
  def searchForPagesByBookId(bookId: String)(implicit mc: MarkerContext): Future[List[PageData]]

  // Delete a book or page by the ID
  def deleteBookById(bookId: String)(implicit mc: MarkerContext): Future[Unit]
  def deletePageById(id: String)(implicit mc: MarkerContext): Future[Unit]

}

/**
  * An Elasticsearch implementation of the repository.
  *
  * A custom execution context is used here to establish that blocking operations should be
  * executed in a different thread than Play's ExecutionContext, which is used for CPU bound tasks
  * such as rendering.
  */
@Singleton
class RepositoryImpl @Inject()()(implicit ec: BoogleExecutionContext) extends Repository {
  import com.sksamuel.elastic4s.http.ElasticDsl._
  import com.sksamuel.elastic4s.http.{RequestFailure, RequestSuccess}
  import com.sksamuel.elastic4s.http.search.SearchResponse

  val logger = Logger(this.getClass)
  val client = ElasticClient(ElasticProperties("http://localhost:9200"))

  override def indexBookData(data: BookData)(implicit mc: MarkerContext): Future[String] = {
    logger.trace(s"index book: data = $data")
    client execute {
      indexInto("book" / "bookType") fields("title" -> data.title, "author" -> data.author)
    } map {
      case failure: RequestFailure =>
        logger.error(s"error indexing book: data = $data, error = $failure")
        throw IndexBookError(failure)
      case success: RequestSuccess[IndexResponse] =>
        success.result.id
    }
  }

  override def indexPageData(data: PageData)(implicit mc: MarkerContext): Future[String] = {
    logger.trace(s"index page: data = $data")
    client execute {
      indexInto("page" / "pageType") fields("bookId" -> data.bookId, "number" -> data.number, "content" -> data.content)
    } map {
      case failure: RequestFailure =>
        logger.error(s"error indexing page: data = $data, error = $failure")
        throw IndexPageError(failure)
      case success: RequestSuccess[IndexResponse] =>
        success.result.id
    }
  }


  override def getBooks()(implicit mc: MarkerContext): Future[List[BookData]] = {
    logger.trace(s"get all books")
    client execute {
      search("book")
    } map {
      case failure: RequestFailure =>
        logger.error(s"error getting all books, error = $failure")
        throw SearchPageError(failure)
      case success: RequestSuccess[SearchResponse] =>
        if (success.result.isEmpty) List()
        else {
          success.result.hits.hits.toList
            .map(book => BookData(book.id, book.sourceField("title").toString, book.sourceField("title").toString))
        }
    }
  }

  override def getBookById(bookId: String)(implicit mc: MarkerContext): Future[Option[BookData]] = {
    logger.trace(s"get book: ID = $bookId")
    client execute {
      get(bookId) from("book" / "bookType")
    } map {
      case failure: RequestFailure =>
        logger.error(s"error getting book: ID = $bookId, error = $failure")
        throw GetBookError(failure)
      case success: RequestSuccess[GetResponse] =>
        if (!success.result.found) None
        else Option(BookData(success.result.id, success.result.sourceField("title").toString, success.result.sourceField("author").toString))
    }
  }

  override def searchForPageByContent(content: String)(implicit mc: MarkerContext): Future[Option[PageData]] = {
    logger.trace(s"search for page by content: search phrase = $content")
    client execute {
      search("page") query content
    } map {
      case failure: RequestFailure =>
        logger.error(s"error searching for page: search phrase = $content, error = $failure")
        throw SearchPageError(failure)
      case success: RequestSuccess[SearchResponse] =>
        if (success.result.isEmpty) None
        else {
          // TODO: order this by '_score' (https://www.elastic.co/guide/en/elasticsearch/reference/current/query-filter-context.html)
          val page = success.result.hits.hits.head
          Option(PageData(page.id, page.sourceField("bookId").toString, page.sourceField("number").toString, page.sourceField("content").toString))
        }
    }
  }

  override def searchForPagesByBookId(bookId: String)(implicit mc: MarkerContext): Future[List[PageData]] = {
    logger.trace(s"search for pages: book ID = $bookId")
    client execute {
      search("page") query bookId
    } map {
      case failure: RequestFailure =>
        logger.error(s"error searching for page: book ID = $bookId, error = $failure")
        throw SearchPageError(failure)
      case success: RequestSuccess[SearchResponse] =>
        if (success.result.isEmpty) List()
        else {
          success.result.hits.hits map { page =>
            PageData(page.id, page.sourceField("bookId").toString, page.sourceField("number").toString, page.sourceField("content").toString)
          } toList
        }
    }
  }

  override def deleteBookById(id: String)(implicit mc: MarkerContext): Future[Unit] = {
    logger.trace(s"delete book: ID = $id")
    client execute {
      delete(id) from("book" / "bookType")
    } map {
      case failure: RequestFailure =>
        logger.error(s"error deleting book: ID = $id, error = $failure")
        throw DeletePageError(failure)
      case success: RequestSuccess[DeleteResponse] => Unit
    }
  }

  override def deletePageById(id: String)(implicit mc: MarkerContext): Future[Unit] = {
    logger.trace(s"delete page: ID = $id")
    client execute {
      delete(id) from("page" / "pageType")
    } map {
      case failure: RequestFailure =>
        logger.error(s"error deleting page: ID = $id, error = $failure")
        throw DeletePageError(failure)
      case success: RequestSuccess[DeleteResponse] => Unit
    }
  }
}