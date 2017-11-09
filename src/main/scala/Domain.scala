import java.util

import org.jsoup.Jsoup

import scala.collection.JavaConverters
import org.jsoup.nodes.Document

import scala.collection.mutable.ArrayBuffer

sealed trait Page
case class DeadEnd(title: String) extends Page
case class ProcessablePage(title: String) extends Page


object Page {

  def fromTitle(title: String): Page =
    title match {
      case deadSpider if title.startsWith("Oh no!") => DeadEnd(deadSpider)
      case _ => ProcessablePage(title)
    }
}


object SpiderBro {
  def apply(url: String): SpiderBro = new SpiderBro(url)

  def findNeighbors(doc: Document): Stream[String] = {
    val java: util.List[String] = doc.body().getElementsByTag("a").eachAttr("href")
    JavaConverters.asScalaBuffer(java).toStream
  }
}
class SpiderBro(val baseUrl: String) {
  import org.jsoup._
  import com.softwaremill.sttp._

  implicit val backend = HttpURLConnectionBackend()

  val name = "SpiderBro"

  val robots = sttp.get(uri"$baseUrl/robots.txt").send()
  val homePage: Document = Jsoup.connect(baseUrl).get()

  val robotsCache = new RobotCache(robots.unsafeBody, name, baseUrl)

  def crawl(links: Stream[Link]) = {
    links.filter(_.isLegalToIndex).foreach { l =>
      Page.fromTitle(l.title) match {
        case DeadEnd(dead) => throw new Error(dead)
        case ProcessablePage(t) =>
          println(l.url)
          println(t)
      }
    }
  }


  def linkStreamFrom(start: Link): Stream[Link] = start #:: start.neighbors.flatMap(linkStreamFrom)

  val linkStream: Stream[Link] = linkStreamFrom(Link(baseUrl, 5))

  crawl(linkStream)


  case class Link(url: String, maxDepth: Double = Double.PositiveInfinity) {
    lazy val response = Jsoup.connect(url).execute()
    lazy val doc =  response.parse

    lazy val metas = JavaConverters.asScalaBuffer(doc.getElementsByTag("meta")).filter(_.attr("name") == "robots")
    lazy val headers = for {
      header <- Option(response.header("x-robots-tag")).toList
      word <- header.split(",").map(_.trim)
    } yield word

    lazy val directives = headers ++ metas.flatMap(_.attr("content").split(",").map(_.trim))

    def title = doc.title

    def isLegalToIndex: Boolean = !robotsCache.isDisallowed(url) && !directives.contains("noindex")
    def isLegalToCrawlFrom: Boolean = !robotsCache.isDisallowed(url) && !directives.contains("nofollow")

    def neighbors: Stream[Link] = if (!isLegalToCrawlFrom || maxDepth <= 0) {
      Stream.empty
    } else {
        val java: util.List[String] = doc.body().getElementsByTag("a").eachAttr("abs:href")
        JavaConverters.asScalaBuffer(java).toStream.map(l => Link(l, maxDepth - 1))
      }
    }

}

object Main extends App {

  val crawler = new SpiderBro("http://localhost:8000")

}

class RobotCache(val robotsTxt: String, val UA: String, baseUrl: String) {

  private val _disallowPages: ArrayBuffer[String] = ArrayBuffer()

  var currentUserAgent: String = ""
  for (line <- robotsTxt.lines) {
    if(line.startsWith("User-agent")) {
      currentUserAgent = line.split(" ").last
    } else if(line.startsWith("Disallow") && (currentUserAgent == UA || currentUserAgent == "*")) {
      val entry = line.split(" ").last
      _disallowPages.append(baseUrl + (if (!entry.endsWith(".html")) s"$entry.html" else entry))
    }
  }


  val disallowPages: List[String] = _disallowPages.toList

  disallowPages.foreach(println)

  def isDisallowed(page: String): Boolean = disallowPages.contains(page)

}