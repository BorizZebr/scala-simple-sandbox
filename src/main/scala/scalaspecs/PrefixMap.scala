package src.scala.scalaspecs

import scala.collection.mutable
import scala.collection.immutable
import scala.collection.mutable.{ListBuffer, Builder, MapBuilder}
import scala.collection.generic.CanBuildFrom

/**
  * Created by borisbondarenko on 29.03.16.
  */
class PrefixMap[T]
  extends mutable.Map[String, T]
    with mutable.MapLike[String, T, PrefixMap[T]] {

  var suffixes: immutable.Map[Char, PrefixMap[T]] = Map.empty
  var value: Option[T] = None

  def get(s: String): Option[T] =
    if (s.isEmpty) value
    else {
      val a = suffixes get s(0)
      a flatMap (_.get(s substring 1))
    }

  def withPrefix(s: String): PrefixMap[T] =
    if (s.isEmpty) this
    else {
      val leading = s(0)
      suffixes get leading match {
        case None =>
          suffixes = suffixes + (leading -> empty)
        case _ =>
      }
      suffixes(leading) withPrefix (s substring 1)
    }

  override def update(s: String, elem: T) =
    withPrefix(s).value = Some(elem)

  override def remove(s: String): Option[T] =
    if (s.isEmpty) { val prev = value; value = None; prev }
    else suffixes get s(0) flatMap (_.remove(s substring 1))

  def iterator: Iterator[(String, T)] =
    (for (v <- value.iterator) yield ("", v)) ++
      (for ((chr, m) <- suffixes.iterator;
            (s, v) <- m.iterator) yield (chr +: s, v))

  def += (kv: (String, T)): this.type = { update(kv._1, kv._2); this }

  def -= (s: String): this.type  = { remove(s); this }

  override def empty = new PrefixMap[T]

  /**
    * Have broken all my head to implement tail recursion, still failing=(
    * @return
    */
  override def toString() = {

    def _toString(sfx: immutable.Map[Char, PrefixMap[T]], res: ListBuffer[String], n: Int): ListBuffer[String] = {

      val level = n + 1
      for (s <- sfx) {
        res += "|-- " * n + s._1 + ": " + s._2.value
        _toString(s._2.suffixes, res, level)
      }

      res
    }

    _toString(this.suffixes, ListBuffer(), 0).toList mkString "\n"
  }
}

object PrefixMap extends {

  def empty[T] = new PrefixMap[T]

  def apply[T](args: (String, T)*): PrefixMap[T] = {
    val res: PrefixMap[T] = empty
    for(a <- args) res += a
    res
  }

  def newBuilder[T]: mutable.Builder[(String, T), PrefixMap[T]] =
    new mutable.MapBuilder[String, T, PrefixMap[T]](empty)

  implicit def canBuildFrom[T]: CanBuildFrom[PrefixMap[_], (String, T), PrefixMap[T]] =
    new CanBuildFrom[PrefixMap[_], (String, T), PrefixMap[T]] {
      def apply(from: PrefixMap[_]) = newBuilder[T]
      def apply() = newBuilder[T]
    }
}
