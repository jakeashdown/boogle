package v1.boogle

import org.scalamock.scalatest.MockFactory
import org.scalatest.TestData
import org.scalatestplus.play._
import org.scalatestplus.play.guice._
import play.api.{Application, MarkerContext}
import play.api.inject._
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json.Json
import play.api.test._
import play.api.test.Helpers._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.language.implicitConversions

class ControllerSpec extends PlaySpec with GuiceOneAppPerTest with MockFactory {

  val mockRepository = mock[Repository]

  trait BookWithId {
    def withId
  }

  // Books and pages will be passed into Repository methods without an ID when indexing
  val booksWithoutId = List(
    BookData(null, "The Dark Tower", "Steven King") -> List( // bookId = "ZkqljmcBYw-dkWcnEEYc"
      PageData(null, "ZkqljmcBYw-dkWcnEEYc", "1", "Roland travels the desert"),
      PageData(null, "ZkqljmcBYw-dkWcnEEYc", "2", "He meets a kid"),
      PageData(null, "ZkqljmcBYw-dkWcnEEYc", "3", "He kills him")
    ),
    BookData(null, "Blindsight", "Peter Watts") -> List( // bookId = "dDxot2cBR-e96N9LlYIz"
      PageData(null, "dDxot2cBR-e96N9LlYIz", "1", "Some weirdos fly to space"),
      PageData(null, "dDxot2cBR-e96N9LlYIz", "2", "They meet some freaky aliens"),
      PageData(null, "dDxot2cBR-e96N9LlYIz", "3", "There's a vampire"),
      PageData(null, "dDxot2cBR-e96N9LlYIz", "4", "It doesn't end well")
    )
  )

  // Books and pages will be returned by Repository methods with an ID searching
  val booksWithId = List(
    BookData("ZkqljmcBYw-dkWcnEEYc", "The Dark Tower", "Steven King") -> List(
      PageData("abcljmcBYw-dkWcnEklm", "ZkqljmcBYw-dkWcnEEYc", "1", "Roland travels the desert"),
      PageData("dfgljmcBYw-dkWcnEtuv", "ZkqljmcBYw-dkWcnEEYc", "2", "He meets a kid"),
      PageData("hijljmcBYw-dkWcnEwxy", "ZkqljmcBYw-dkWcnEEYc", "3", "He kills him")
    ),
    BookData("dDxot2cBR-e96N9LlYIz", "Blindsight", "Peter Watts") -> List(
      PageData("ABCot2cBR-e96N9LlYNO", "dDxot2cBR-e96N9LlYIz", "1", "Some weirdos fly to space"),
      PageData("DFGot2cBR-e96N9LlYPQ", "dDxot2cBR-e96N9LlYIz", "2", "They meet some freaky aliens"),
      PageData("HIJot2cBR-e96N9LlYRS", "dDxot2cBR-e96N9LlYIz", "3", "There's a vampire"),
      PageData("KLMot2cBR-e96N9LlYTU", "dDxot2cBR-e96N9LlYIz", "4", "It doesn't end well")
    )
  )

  // Override this to bind Repository to a mock
  implicit override def newAppForTest(testData: TestData): Application = {
    GuiceApplicationBuilder()
      .overrides(bind[Repository].toInstance(mockRepository))
      .build()
  }

  "Controller" should {

    "list all indexed books" in {
      // Setup the mock repository methods
      (mockRepository.getBooks()(_: MarkerContext))
        .expects(*)
        .returning(Future(List(booksWithId(0)._1, booksWithId(1)._1)))

      (mockRepository.searchForPagesByBookId(_: String)(_: MarkerContext))
        .expects("ZkqljmcBYw-dkWcnEEYc", *)
        .returning(Future(booksWithId(0)._2))
      (mockRepository.searchForPagesByBookId(_: String)(_: MarkerContext))
        .expects("dDxot2cBR-e96N9LlYIz", *)
        .returning(Future(booksWithId(1)._2))

      // Make the request
      val request = FakeRequest(GET, "/v1/book").withHeaders(HOST -> "localhost:9000")
      val books = route(app, request).get

      val booksJson = Json.parse("""[{
              "author": "Steven King",
              "id": "ZkqljmcBYw-dkWcnEEYc",
              "pages": [
                [1, "Roland travels the desert"],
                [2, "He meets a kid"],
                [3, "He kills him"]
              ],
              "title": "The Dark Tower"
          },
          {
              "author": "Peter Watts",
              "id": "dDxot2cBR-e96N9LlYIz",
                            "pages": [
                [1, "Some weirdos fly to space"],
                [2, "They meet some freaky aliens"],
                [3, "There's a vampire"],
                [4, "It doesn't end well"]
              ],
              "title": "Blindsight"
          }]""")
      assert(contentAsJson(books) equals booksJson)
    }

    "index a book for search and return the ID" in {
      // Setup the mock repository methods
      (mockRepository.indexBookData(_: BookData)(_: MarkerContext))
        .expects(booksWithoutId.head._1, *)
        .returning(Future("ZkqljmcBYw-dkWcnEEYc"))

      (mockRepository.indexPageData(_: PageData)(_: MarkerContext))
        .expects(booksWithoutId.head._2(0), *)
        .returning(Future("AkqljmcBYw-dkWcnEEYx"))
      (mockRepository.indexPageData(_: PageData)(_: MarkerContext))
        .expects(booksWithoutId.head._2(1), *)
        .returning(Future("BkqljmcBYw-dkWcnEEYy"))
      (mockRepository.indexPageData(_: PageData)(_: MarkerContext))
        .expects(booksWithoutId.head._2(2), *)
        .returning(Future("CkqljmcBYw-dkWcnEEYz"))

      // Post a book with three pages
      val request = FakeRequest(POST, "/v1/book")
        .withHeaders(HOST -> "localhost:9000")
        .withJsonBody(Json.parse("""{
              "author": "Steven King",
              "pages[0]": "Roland travels the desert",
              "pages[1]": "He meets a kid",
              "pages[2]": "He kills him",
              "title": "The Dark Tower"
          }"""))

      val id = route(app, request).get
      assert(contentAsString(id) equals "ZkqljmcBYw-dkWcnEEYc")
    }

  }

}