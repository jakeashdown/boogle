package v1.boogle

import javax.inject.{Inject, Singleton}
import akka.actor.ActorSystem
import com.sksamuel.elastic4s.http.{ElasticClient, ElasticProperties, RequestFailure, Response}
import com.sksamuel.elastic4s.http.delete.DeleteResponse
import com.sksamuel.elastic4s.http.get.GetResponse
import com.sksamuel.elastic4s.http.index.IndexResponse
import play.api.libs.concurrent.CustomExecutionContext
import play.api.{Logger, MarkerContext}

import scala.concurrent.Future

final case class BookData(id: String, title: String, author: String)
final case class PageData(id: String, bookTitle: String, bookId: String, number: String, content: String)

final case class IndexBookError(failure: RequestFailure) extends Throwable
final case class IndexPageError(failure: RequestFailure) extends Throwable

final case class GetBookError(failure: RequestFailure) extends Throwable

final case class SearchPageError(failure: RequestFailure) extends Throwable

class BoogleExecutionContext @Inject()(actorSystem: ActorSystem) extends CustomExecutionContext(actorSystem, "repository.dispatcher")

/**
  * A pure non-blocking interface for the Repository.
  */
trait Repository {
  // Return the ID of the indexed book or page
  def indexBookData(data: BookData)(implicit mc: MarkerContext): Future[String]
  def indexPageData(data: PageData)(implicit mc: MarkerContext): Future[String]

  // Gets the book by ID, if it exists
  def getBookById(bookId: String)(implicit mc: MarkerContext): Future[Option[BookData]]

  // Return the page matching the search phrase, including the ID and title of the book
  def getPageDataBySearchPhrase(searchPhrase: String)(implicit mc: MarkerContext): Future[Option[PageData]]

  // Delete all pages, and the book itself, associated with a given ID
  def deleteBookByBookId(bookId: String)(implicit mc: MarkerContext): Future[Boolean]
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
    client.execute {
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
    client.execute {
      indexInto("page" / "pageType") fields("bookId" -> data.bookId, "number" -> data.number, "content" -> data.content)
    } map {
      case failure: RequestFailure =>
        logger.error(s"error indexing page: data = $data, error = $failure")
        throw IndexPageError(failure)
      case success: RequestSuccess[IndexResponse] =>
        success.result.id
    }
  }

  override def getBookById(bookId: String)(implicit mc: MarkerContext): Future[Option[BookData]] = {
    logger.trace(s"get book: ID = $bookId")
    client.execute {
      get(bookId) from("book" / "bookType")
    } map {
      case failure: RequestFailure =>
        logger.error(s"error getting book: ID = $bookId, error = $failure")
        throw GetBookError(failure)
      case success: RequestSuccess[GetResponse] =>
        if (!success.result.found) None
        else {
          val data = BookData(success.result.id, success.result.sourceField("title").toString, success.result.sourceField("author").toString)
          Option(data)
        }
    }
  }

  override def getPageDataBySearchPhrase(searchPhrase: String)(implicit mc: MarkerContext): Future[Option[PageData]] = {
    logger.trace(s"get page data: searchPhrase = $searchPhrase")
    client.execute {
      search("page") query searchPhrase
    } flatMap {
      case _: RequestFailure => Future(None)
      case success: RequestSuccess[SearchResponse] =>
        if (success.result.hits.hits.length == 0) Future(None)
        else {
          val page = success.result.hits.hits.head
          client.execute {
            get(page.sourceField("bookId").toString) from("book" / "bookType")
          } map { bookResponse =>
            Option(PageData(page.id,
              bookResponse.result.sourceField("title").toString,
              page.sourceField("bookId").toString,
              page.sourceField("number").toString,
              page.sourceField("content").toString)
            )
          }
        }
    }
  }

  override def deleteBookByBookId(bookId: String)(implicit mc: MarkerContext): Future[Boolean] = {
    logger.trace(s"delete book: bookId = $bookId")
    // Get all page IDs associated with this book
    client.execute {
      search("page") query bookId
    } flatMap {
      case _: RequestFailure => Future(false)
      case success: RequestSuccess[SearchResponse] =>
        val pageIds = success.result.hits.hits
        val deletePageFutures = for { id <- pageIds.map(_.id).toList }
          yield client.execute {
            delete(id) from("page" / "pageType")
          }
        Future.sequence(deletePageFutures)
    } flatMap { case deleteResponses: List[Response[_]] =>
        val deleteFailures = deleteResponses.filter {
          case failure: RequestFailure => true
          case success: RequestSuccess[DeleteResponse] => false
        }
        if (deleteFailures.nonEmpty) Future(false)
        else {
          client.execute {
            delete(bookId) from("book" / "bookType")
          } map {
            case _: RequestFailure => false
            case _: RequestSuccess[_] => true
          }
        }
    }
  }
}