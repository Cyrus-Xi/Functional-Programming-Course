package objsets

import common._
import TweetReader._

/**
 * A class to represent tweets.
 */
class Tweet(val user: String, val text: String, val retweets: Int) {
  override def toString: String =
    "User: " + user + "\n" +
    "Text: " + text + " [" + retweets + "]"
}

/**
 * This represents a set of objects of type `Tweet` in the form of a binary search
 * tree. Every branch in the tree has two children (two `TweetSet`s). 
 *
 * Note that the above structure requires us to be able to compare two tweets (we
 * need to be able to say which of two tweets is larger, or if they are equal). In
 * this implementation, the equality / order of tweets is based on the tweet's text
 * (see `def incl`). Hence, a `TweetSet` could not contain two tweets with the same
 * text from different users.
 */
abstract class TweetSet {

  /**
   * This method takes a predicate and returns a subset of all the elements
   * in the original set for which the predicate is true.
   *
   * Can be implemented here because accumulator will always begin empty.
   */
  def filter(p: Tweet => Boolean): TweetSet = filterAcc(p, new Empty)

  /**
   * Helper method for `filter` that propagates the accumulated tweets.
   */
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet

  /**
   * Returns a new `TweetSet` that is the union of `TweetSet`s `this` and `that`.
   */
   def union(that: TweetSet): TweetSet

   /**
    * To conveniently differentiate between the two sub-classes.
    */
   def isEmpty: Boolean
   
  /**
   * Returns the tweet from this set which has the greatest retweet count.
   */
  def mostRetweeted: Tweet

  /**
   * Returns a list containing all tweets of this set, sorted by retweet count
   * in descending order. In other words, the head of the resulting list should
   * have the highest retweet count.
   */
  def descendingByRetweet: TweetList

  /**
   * Returns a new `TweetSet` which contains all elements of this set, and the
   * the new element `tweet` in case it does not already exist in this set.
   *
   * If `this.contains(tweet)`, the current set is returned.
   */
  def incl(tweet: Tweet): TweetSet

  /**
   * Returns a new `TweetSet` which excludes `tweet`.
   */
  def remove(tweet: Tweet): TweetSet

  /**
   * Tests if `tweet` exists in this `TweetSet`.
   */
  def contains(tweet: Tweet): Boolean

  /**
   * This method takes a function and applies it to every element in the set.
   */
  def foreach(f: Tweet => Unit): Unit
}

class Empty extends TweetSet {

  /**
   * Helper function for filter. Doesn't add to accumulator, just returns it.
   */
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = acc
  
  /**
   * If empty, then the elements in either set are just those in that.
   */
  def union(that: TweetSet): TweetSet = that
  
  def isEmpty: Boolean = true
  
  def mostRetweeted = throw new NoSuchElementException("Empty set")
  
  /**
   * Empty set returns empty list.
   */
  def descendingByRetweet: TweetList = Nil
  
  def contains(tweet: Tweet): Boolean = false

  def incl(tweet: Tweet): TweetSet = new NonEmpty(tweet, new Empty, new Empty)

  def remove(tweet: Tweet): TweetSet = this

  def foreach(f: Tweet => Unit): Unit = ()
}

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {

  /**
   * Helper function for filter. Recursive; goes down left side first then right.
   */
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = {
    if (p(elem)) right.filterAcc(p, left.filterAcc(p, acc.incl(elem)))
    else right.filterAcc(p, left.filterAcc(p, acc))
  }

  /**
   * Tail recursive.
   */
  def union(that: TweetSet): TweetSet = right.union(left.union(that.incl(elem)))
  
  def isEmpty: Boolean = false

  /**
   * Helper function for descendingByRetweet. Returns most retweeted Tweet in set.
   */
  def mostRetweeted: Tweet = {
    // Helper function to compare Tweets by number of retweets.
    def moreRetweeted(curr: Tweet, other: Tweet): Tweet = {
      if (curr.retweets >= other.retweets) curr else other
    }
    // Use isEmpty methods for convenience and proceed recursively.
    if (left.isEmpty && right.isEmpty) elem
    else if (left.isEmpty) moreRetweeted(elem, right.mostRetweeted)
    else if (right.isEmpty) moreRetweeted(elem, left.mostRetweeted)
    else moreRetweeted(moreRetweeted(elem, left.mostRetweeted), right.mostRetweeted)
  }
 
  /**
   * Get most retweeted Tweet in set. Remove that tweet and add to result list by 
   * creating a new Cons. After that, repeat recursively with the now smaller set.
   */
  def descendingByRetweet: TweetList = {
    new Cons(this.mostRetweeted, this.remove(this.mostRetweeted).descendingByRetweet)
  }
  
  /**
   * If "bottoms out" to Empty, then know that false.
   */
  def contains(x: Tweet): Boolean =
    if (x.text < elem.text) left.contains(x)
    else if (elem.text < x.text) right.contains(x)
    else true

  def incl(x: Tweet): TweetSet = {
    if (x.text < elem.text) new NonEmpty(elem, left.incl(x), right)
    else if (elem.text < x.text) new NonEmpty(elem, left, right.incl(x))
    else this
  }

  def remove(tw: Tweet): TweetSet =
    if (tw.text < elem.text) new NonEmpty(elem, left.remove(tw), right)
    else if (elem.text < tw.text) new NonEmpty(elem, left, right.remove(tw))
    else left.union(right)

  def foreach(f: Tweet => Unit): Unit = {
    f(elem)
    left.foreach(f)
    right.foreach(f)
  }
}

trait TweetList {
  def head: Tweet
  def tail: TweetList
  def isEmpty: Boolean
  def foreach(f: Tweet => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }
}

object Nil extends TweetList {
  def head = throw new java.util.NoSuchElementException("head of EmptyList")
  def tail = throw new java.util.NoSuchElementException("tail of EmptyList")
  def isEmpty = true
}

class Cons(val head: Tweet, val tail: TweetList) extends TweetList {
  def isEmpty = false
}


object GoogleVsApple {
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  // The tweets that contain a keyword in google.
  lazy val googleTweets: TweetSet = {
    TweetReader.allTweets.filter((x: Tweet) => google.exists(x.text.contains))
  }
  lazy val appleTweets: TweetSet = {
    TweetReader.allTweets.filter((x: Tweet) => apple.exists(x.text.contains))
  }

  /**
   * A list of all tweets mentioning a keyword from either apple or google,
   * sorted by the number of retweets.
   */
  lazy val trending: TweetList = googleTweets.union(appleTweets).descendingByRetweet
}

object Main extends App {
  // Print the trending tweets
  GoogleVsApple.trending foreach println
}
