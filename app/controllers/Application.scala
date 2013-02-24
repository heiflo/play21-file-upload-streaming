package controllers

import play.api._
import mvc._
import services.StreamingBodyParser.streamingBodyParser
import java.io.{FileOutputStream, File}

object Application extends Controller {
  val welcomeMsg = "Play Scala File Upload"

  def index = Action {
    Ok(views.html.index(welcomeMsg))
  }

  /** Higher-order function that accepts the unqualified name of the file to stream to and returns the output stream
    * for the new file. This example streams to a file, but streaming to AWS S3 is also possible */
  def streamConstructor(filename: String) = {
    val dir = new File(sys.env("HOME"), "uploadedFiles")
    dir.mkdirs()
    Option(new FileOutputStream(new File(dir, filename)))
  }

  def upload = Action(streamingBodyParser(streamConstructor)) { request =>
    val params = request.body.asFormUrlEncoded // you can extract request parameters for whatever your app needs
    val result = request.body.files(0).ref
    if (result.isRight) { // streaming succeeded
      val filename = result.right.get.filename
      Ok(s"File $filename successfully streamed.")
    } else { // file streaming failed
      Ok(s"Streaming error occurred: ${result.left.get.errorMessage}")
    }
  }
}
