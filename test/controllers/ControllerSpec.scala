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

class ControllerSpec extends PlaySpec with GuiceOneAppPerTest with MockFactory {

  // Override newAppForTest to bind Repository to a mock
  implicit override def newAppForTest(testData: TestData): Application = {
    // Setup the mocked repository
    val mockRepository = mock[Repository]

    (mockRepository.indexBookData(_: BookData)(_: MarkerContext))
      .expects(BookData(null, "The Dark Tower", "Steven King"), *)
      .returning(Future("ZkqljmcBYw-dkWcnEEYc"))

    (mockRepository.indexPageData(_: PageData)(_: MarkerContext))
      .expects(PageData(null, "ZkqljmcBYw-dkWcnEEYc", "1", "Roland travels the desert"), *)
      .returning(Future("AkqljmcBYw-dkWcnEEYx"))
    (mockRepository.indexPageData(_: PageData)(_: MarkerContext))
      .expects(PageData(null, "ZkqljmcBYw-dkWcnEEYc", "2", "He meets a kid"), *)
      .returning(Future("BkqljmcBYw-dkWcnEEYy"))
    (mockRepository.indexPageData(_: PageData)(_: MarkerContext))
      .expects(PageData(null, "ZkqljmcBYw-dkWcnEEYc", "3", "He kills him"), *)
      .returning(Future("CkqljmcBYw-dkWcnEEYz"))

    GuiceApplicationBuilder()
      .overrides(bind[Repository].toInstance(mockRepository))
      .build()
  }

  "Controller" should {
    
    "index a book for search and return the ID" in {
      // Post a book with three pages
      val json =
        """
          {
              "author": "Steven King",
              "pages[0]": "Roland travels the desert",
              "pages[1]": "He meets a kid",
              "pages[2]": "He kills him",
              "title": "The Dark Tower"
          }
        """
      val request = FakeRequest(POST, "/v1/book")
        .withHeaders(HOST -> "localhost:9000")
        .withJsonBody(Json.parse(json))

      val id = route(app, request).get
      assert(contentAsString(id) equals "ZkqljmcBYw-dkWcnEEYc")
    }

  }

}