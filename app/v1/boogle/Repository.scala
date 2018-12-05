package v1.boogle

import javax.inject.{Inject, Singleton}
import akka.actor.ActorSystem
import play.api.libs.concurrent.CustomExecutionContext
import play.api.{Logger, MarkerContext}

import scala.concurrent.Future

final case class BookData(id: String, title: String, author: String, pages: List[String])
final case class PageData(id: String, bookTitle: String, bookId: String, number: String, content: String)

class BoogleExecutionContext @Inject()(actorSystem: ActorSystem) extends CustomExecutionContext(actorSystem, "repository.dispatcher")

/**
  * A pure non-blocking interface for the Repository.
  */
trait Repository {
  def indexBookData(data: BookData)(implicit mc: MarkerContext): Future[String]
  def getPageDataBySearchPhrase(searchPhrase: String)(implicit mc: MarkerContext): Future[Option[PageData]]
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
  import com.sksamuel.elastic4s.embedded.LocalNode
  import com.sksamuel.elastic4s.http.ElasticDsl._
  import com.sksamuel.elastic4s.http.{RequestFailure, RequestSuccess}
  import com.sksamuel.elastic4s.http.search.SearchResponse

  val logger = Logger(this.getClass)

  // In production, we wouldn't be using this cluster on the local filesystem, or need to create these indexes
  val localNode = LocalNode("mycluster", "/tmp/datapath/6")
  val client = localNode.client(shutdownNodeOnClose = true)
  client.execute {
    createIndex("book").mappings(mapping("bookType").fields(
      textField("title"), textField("author")
    ))
  }
  client.execute {
    createIndex("book").mappings(mapping("bookType").fields(
      textField("title"), textField("author")
    ))
  }

  override def indexBookData(data: BookData)(implicit mc: MarkerContext): Future[String] = {
    logger.trace(s"index book: data = $data")
    client.execute {
      indexInto("book" / "bookType") fields("title" -> data.title, "author" -> data.author)
    } map { response =>
      val bookId = response.result.id
      val pageFutures = for { (content, index) <- data.pages.zipWithIndex }
        yield {
          client.execute {
            indexInto("page" / "pageType")
              .fields("bookId" -> bookId, "number" -> (index + 1), "content" -> content)
          }
        }
      bookId
    }
  }

  override def getPageDataBySearchPhrase(searchPhrase: String)(implicit mc: MarkerContext): Future[Option[PageData]] = {
    logger.trace(s"get page data: searchPhrase = $searchPhrase")
    client.execute {
      search("page") query fuzzyQuery("content", searchPhrase)
    } flatMap {
      case _: RequestFailure => Future(None)
      case success: RequestSuccess[SearchResponse] =>
        if (success.result.hits.hits.length == 0) Future(None)
        else {
          val page = success.result.hits.hits.head
          client.execute {
            get(page.sourceField("bookId").toString).from("book" / "bookType")
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
      search("page") query termQuery("bookId", bookId)
    }.await


    Future(true)
  }
}