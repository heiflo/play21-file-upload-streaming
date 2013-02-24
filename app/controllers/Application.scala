package controllers

import play.api._
import libs.iteratee._
import mvc.MultipartFormData.FilePart
import mvc._
import java.io.{File, FileOutputStream, OutputStream}
import scala.{Right, Left, Either}
import scala.Some
import play.api.Logger

object Application extends Controller {

  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }

  def upload = Action(streamingBodyParser) { request =>
      // get request parameters
      val params = request.body.asFormUrlEncoded
      if (request.body.files(0).ref.isRight) { // streaming succeeded
        val success: StreamingSuccess = request.body.files(0).ref.right.get
        val filename = success.filename
        Ok(s"File $filename successfully streamed.")
      } else { // file streaming failed
        val error: StreamingError = request.body.files(0).ref.left.get
        Ok(s"Streaming error occurred: ${error.errorMessage}")
      }
  }

  case class StreamingSuccess(filename: String)
  case class StreamingError(errorMessage: String)

  val streamingBodyParser = BodyParser { request =>
    // Use Play's existing multipart parser from play.api.mvc.BodyParsers.
    // Note that the RequestHeader object is wrapped here so it can be accessed in streamingFilePartHandler method
    parse.multipartFormData(streamingFilePartHandler(request)).apply(request)
  }

  // custom implementation of a PartHandler, inspired by these Play mailing list threads:
  // https://groups.google.com/forum/#!searchin/play-framework/PartHandler/play-framework/WY548Je8VB0/dJkj3arlBigJ
  // https://groups.google.com/forum/#!searchin/play-framework/PartHandler/play-framework/n7yF6wNBL_s/wBPFHBBiKUwJ
  def streamingFilePartHandler(request: RequestHeader): BodyParsers.parse.Multipart.PartHandler[FilePart[Either[StreamingError, StreamingSuccess]]] = {
    parse.Multipart.handleFilePart {
      case parse.Multipart.FileInfo(partName, filename, contentType) =>
        // Reference to hold the error message
        var errorMsg: Option[StreamingError] = None
        // Following the output stream you'll write to. In case something goes wrong while trying to instantiate
        // the output stream, assign the error message to the result reference, e.g.
        // result = Some(StreamingError("network error"))
        // and set the outputStream reference to None -> the Iteratee will then do nothing
        // and the error message will be passed to the Action.
        // Please bear in mind that even if the Iteratee does nothing when the outputStream = None
        // the whole HTTP request will still be processed (there is no way to cancel an HTTP request
        // from the server side).
        val outputStream: Option[OutputStream] =
          try { // This example streams to a file
            val dir = new File(sys.env("HOME"), "uploadedFiles")
            dir.mkdirs()
            Option(new FileOutputStream(new File(dir, filename)))
          } catch {
            case e: Exception => {
              Logger.error(e.getMessage)
              errorMsg = Some(StreamingError(e.getMessage))
              None
            }
          }

        // The fold method that actually does the parsing of the multipart file part.
        // Type A is expected to be Option[OutputStream]
        def fold[E, A](state: A)(f: (A, E) => A): Iteratee[E, A] = {
          def step(s: A)(i: Input[E]): Iteratee[E, A] = i match {
            case Input.EOF => Done(s, Input.EOF)
            case Input.Empty => Cont[E, A](i => step(s)(i))
            case Input.El(e) => {
              val s1 = f(s, e)
              errorMsg match { // if an error occurred during output stream initialisation, set Iteratee to Done
                case Some(result) => Done(s, Input.EOF)
                case None => Cont[E, A](i => step(s1)(i))
              }
            }
          }
          (Cont[E, A](i => step(state)(i)))
        }

        fold[Array[Byte], Option[OutputStream]](outputStream) { (os, data) =>
          os foreach { _.write(data) }
          os
        }.mapDone { os =>
          os foreach { _.close }
          errorMsg match {
            // streaming failed - Left is returned
            case Some(result) => {
              Logger.error(s"Streaming the file $filename failed: ${result.errorMessage}")
              Left(result)
            }
            // streaming succeeded - Right is returned
            case None => {
              Logger.info(s"$filename finished streaming.")
              Right(StreamingSuccess(filename))
            }
          }
        }
    }
  }
}
