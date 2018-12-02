package v1.boogle

import javax.inject.{Inject, Singleton}
import akka.actor.ActorSystem
import play.api.libs.concurrent.CustomExecutionContext
import play.api.{Logger, MarkerContext}

import scala.concurrent.Future

final case class BookData(id: String, title: String, author: String, pages: Map[Int, String])
final case class PageData(id: String, bookTitle: String, bookId: String, number: String, content: String)

class BoogleExecutionContext @Inject()(actorSystem: ActorSystem) extends CustomExecutionContext(actorSystem, "repository.dispatcher")

/**
  * A pure non-blocking interface for the Repository.
  */
trait Repository {
  def indexBook(data: BookData)(implicit mc: MarkerContext): Future[String]
  def getPageBySearchPhrase(searchPhrase: String)(implicit mc: MarkerContext): Future[Option[PageData]]
  def deleteBook(bookId: String)(implicit mc: MarkerContext): Future[Boolean]
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

  val logger = Logger(this.getClass)

  // In production, we wouldn't be using this cluster on the local filesystem,
  // or need to create these indexes
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

  override def indexBook(data: BookData)(implicit mc: MarkerContext): Future[String] = {
    logger.trace(s"index book: data = $data")
    client.execute {
      indexInto("book" / "bookType")
        .fields("title" -> data.title, "author" -> data.author)
    } map { response =>
      val bookId = response.result.id
      data.pages.keySet.map( number =>
        client.execute {
          indexInto("page" / "pageType")
            .fields("bookId" -> bookId, "number" -> number, "content" -> data.pages(number))
        })
      bookId
    }
  }

  override def getPageBySearchPhrase(searchPhrase: String)(implicit mc: MarkerContext): Future[Option[PageData]] = {
    logger.trace(s"get book by search phrase: $searchPhrase")
    client.execute {
      search("page") query fuzzyQuery("content", searchPhrase)
    } map (pageResponse =>
      if (pageResponse.result.hits.hits.size == 0) None
      else {
        val page = pageResponse.result.hits.hits.head
        client.execute {
          get(page.sourceField("bookId").toString).from("book" / "bookType")
        } map ( bookResponse =>
          Option(PageData(page.id,
            bookResponse.result.sourceField("title").toString,
            page.sourceField("bookId").toString,
            page.sourceField("number").toString,
            page.sourceField("content").toString)
          )
        ) await
      }
    )
  }

  override def deleteBook(bookId: String)(implicit mc: MarkerContext): Future[Boolean] = {
    logger.trace(s"delete book: bookId = $bookId")
    // Get all page IDs associated with this book
    client.execute {
      search("page") query termQuery("bookId", bookId)
    }.await


    Future(true)
  }
}