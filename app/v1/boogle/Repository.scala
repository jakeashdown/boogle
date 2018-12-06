package v1.boogle

import javax.inject.{Inject, Singleton}
import akka.actor.ActorSystem
import com.sksamuel.elastic4s.http.{ElasticClient, ElasticProperties, Response}
import com.sksamuel.elastic4s.http.delete.DeleteResponse
import com.sksamuel.elastic4s.http.get.GetResponse
import play.api.libs.concurrent.CustomExecutionContext
import play.api.{Logger, MarkerContext}

import scala.concurrent.Future

final case class BookData(id: String, title: String, author: String, pages: List[String])
final case class PageData(id: String, bookTitle: String, bookId: String, number: String, content: String)

final case class NoSuchBook() extends Throwable

class BoogleExecutionContext @Inject()(actorSystem: ActorSystem) extends CustomExecutionContext(actorSystem, "repository.dispatcher")

/**
  * A pure non-blocking interface for the Repository.
  */
trait Repository {
  // Return the ID of the indexed book or page
  def indexBookData(data: BookData)(implicit mc: MarkerContext): Future[String]
  def indexPageData(data: PageData)(implicit mc: MarkerContext): Future[String]

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
    } map { response =>
      val bookId = response.result.id
      for { (content, index) <- data.pages.zipWithIndex }
        yield {
          client.execute {
            indexInto("page" / "pageType")
              .fields("bookId" -> bookId, "number" -> (index + 1), "content" -> content)
          }
        }
      bookId
    }
  }

  override def indexPageData(data: PageData)(implicit mc: MarkerContext): Future[String] = {
    logger.trace(s"index page: data = $data")
    // Check that the given book exists
    client.execute {
      get("book", "bookType", data.bookId)
    } flatMap {
      case _: RequestFailure => throw NoSuchBook()
      case success: RequestSuccess[GetResponse] =>
        if (!success.result.found) throw NoSuchBook()
        else {
          // Index the page
          client.execute {
            indexInto("page" / "pageType")
              .fields("bookId" -> data.bookId, "number" -> data.number, "content" -> data.content)
          } map(_.result.id)
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