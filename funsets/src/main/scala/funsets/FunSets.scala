package funsets

import common._

/**
 * 2. Purely Functional Sets.
 */
object FunSets {
  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
  type Set = Int => Boolean

  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: Set, elem: Int): Boolean = s(elem)

  /**
   * Returns the set of the one given element.
   */
  def singletonSet(elem: Int): Set = (x: Int) => (x == elem)

  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
  def union(s: Set, t: Set): Set = (x: Int) => s(x) || t(x)

  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` and `t`.
   */
  def intersect(s: Set, t: Set): Set = (x: Int) => s(x) && t(x)

  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
  def diff(s: Set, t: Set): Set = (x: Int) => s(x) && !t(x)

  /**
   * Returns the subset of `s` for which `p` holds.
   */
  def filter(s: Set, p: Int => Boolean): Set = (x: Int) => s(x) && p(x)

  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1000

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   */
  def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      // If reach upper bound, must be true.
      if (a > bound) true
      // If in set and fail predicate, then know that false.
      else if (contains(s, a) && !p(a)) false
      // Else go to next int.
      else iter(a + 1)
    }
    // Start at bottom bound. 
    iter(-bound)
  }

  /**
   * Returns whether there exists at least 1 bounded integer within `s`
   * that satisfies `p`.
   * 
   * An element of s satisfies p if not all elements fail p. That is, if it is 
   * not the case that for all x in s, x is not p, then it must be the case that 
   * there is some x in s that *is* p.
   */
  def exists(s: Set, p: Int => Boolean): Boolean = !forall(s, (x: Int) => !p(x))

  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   * 
   * y is in the resulting set if it can be had from applying f to some x in s.
   */
  def map(s: Set, f: Int => Int): Set = (y: Int) => exists(s, (x: Int) => f(x) == y)

  /**
   * Displays the contents of a set
   */
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: Set) {
    println(toString(s))
  }
}
