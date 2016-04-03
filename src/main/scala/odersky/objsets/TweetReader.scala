package odersky.objsets

object TweetReader {

  object ParseTweets {

    import argonaut._, Argonaut._

    def getTweetData(json: String): List[Tweet] =
      Parse.decodeValidation[List[Tweet]](json).getOrElse(List())

    implicit def TweetCodec: CodecJson[Tweet] = codec3(
      (user: String, text: String, retweets: Int) => new Tweet(user, text, retweets),
      (tweet: Tweet) => (tweet.user, tweet.text, tweet.retweets)
    )("user", "text", "retweets")
  }

  def toTweetSet(l: List[Tweet]): TweetSet = {
    l.foldLeft(new Empty: TweetSet)(_.incl(_))
  }

  def unparseToData(tws: List[Tweet]): String = {
    val buf = new StringBuffer
    for (tw <- tws) {
      val json = "{ \"user\": \"" + tw.user + "\", \"text\": \"" +
        tw.text.replaceAll(""""""", "\\\\\\\"") + "\", \"retweets\": " +
        tw.retweets + ".0 }"
      buf.append(json + ",\n")
    }
    buf.toString
  }

  val sites = List("gizmodo", "TechCrunch", "engadget", "amazondeals", "CNET", "gadgetlab", "mashable")

  private val gizmodoTweets = TweetReader.ParseTweets.getTweetData(TweetData.gizmodo)
  private val techCrunchTweets = TweetReader.ParseTweets.getTweetData(TweetData.TechCrunch)
  private val engadgetTweets = TweetReader.ParseTweets.getTweetData(TweetData.engadget)
  private val amazondealsTweets = TweetReader.ParseTweets.getTweetData(TweetData.amazondeals)
  private val cnetTweets = TweetReader.ParseTweets.getTweetData(TweetData.CNET)
  private val gadgetlabTweets = TweetReader.ParseTweets.getTweetData(TweetData.gadgetlab)
  private val mashableTweets = TweetReader.ParseTweets.getTweetData(TweetData.mashable)

  private val sources = List(gizmodoTweets, techCrunchTweets, engadgetTweets, amazondealsTweets, cnetTweets, gadgetlabTweets, mashableTweets)

  val tweetMap: Map[String, List[Tweet]] =
    Map() ++ Seq(sites(0) -> gizmodoTweets,
      sites(1) -> techCrunchTweets,
      sites(2) -> engadgetTweets,
      sites(3) -> amazondealsTweets,
      sites(4) -> cnetTweets,
      sites(5) -> gadgetlabTweets,
      sites(6) -> mashableTweets)

  val tweetSets: List[TweetSet] = sources.map(tweets => toTweetSet(tweets))

  private val siteTweetSetMap: Map[String, TweetSet] =
    Map() ++ (sites zip tweetSets)

  private def unionOfAllTweetSets(curSets: List[TweetSet], acc: TweetSet): TweetSet =
    if (curSets.isEmpty) acc
    else unionOfAllTweetSets(curSets.tail, acc.union(curSets.head))

  val allTweets: TweetSet = unionOfAllTweetSets(tweetSets, new Empty)
}
