import org.scalatestplus.play._
import org.scalatestplus.play.guice._
import play.api.libs.json.Json
import play.api.test._
import play.api.test.Helpers._

class ControllerSpec extends PlaySpec with GuiceOneAppPerTest {

  "Controller" should {

    "index a book for search" in {
      val json =
        """
          {
              "author": "Steven King",
              "pages[0]": "Roland runs across the desert",
              "pages[1]": "He meets a kid",
              "pages[2]": "He kills him",
              "title": "The dark tower"
          }
        """.stripMargin
      val request = FakeRequest(POST, "/v1/book").withHeaders(HOST -> "localhost:9000")
        .withJsonBody(Json.parse(json))

      val id = route(app, request).get
      contentAsString(id) must not be empty
    }

  }

}