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

  // Override this to bind Repository to a mock
  implicit override def newAppForTest(testData: TestData): Application = {
    GuiceApplicationBuilder()
      .overrides(bind[Repository].toInstance(mockRepository))
      .build()
  }

  "Controller" should {

    "index a book for search and return the ID" in {
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

      // Setup the mock repository methods
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

      val id = route(app, request).get
      assert(contentAsString(id) equals "ZkqljmcBYw-dkWcnEEYc")
    }

  }

}