package controllers

import play.api._
import mvc._
import services.{StreamingSuccess, StreamingError, StreamingBodyParser}
import StreamingBodyParser.streamingBodyParser

object Application extends Controller {
  val welcomeMsg = "Play Scala File Upload"

  def index = Action {
    Ok(views.html.index(welcomeMsg))
  }

  def upload = Action(streamingBodyParser) { request =>
    val params = request.body.asFormUrlEncoded // you can use request parameters if you want
    val result = request.body.files(0).ref
    if (result.isRight) { // streaming succeeded
      val filename = result.right.get.filename
      Ok(s"File $filename successfully streamed.")
    } else { // file streaming failed
      Ok(s"Streaming error occurred: ${result.left.get.errorMessage}")
    }
  }
}
