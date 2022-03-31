package u04lab.polyglot.a01b
import u04lab.polyglot.OptionToOptional
import u04lab.code.Option
import u04lab.code.Option.*
import u04lab.polyglot.Pair
import u04lab.code.Stream
import u04lab.code.List
import u04lab.code.Stream.*
import u04lab.code.List.{flatMap}

import scala.util.Random

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a01b/sol2/ */
object LogicsImpl:
  private val Offset = 3

class LogicsImpl(private val size: Int, private val mines: Int) extends Logics:

  private var minesSet: Set[(Int, Int)] = Set()
  private var selected: Set[(Int, Int)] = Set()
  private var gridSize: Int = size

  private val random = Random()
  while (minesSet.size != mines)
    minesSet += Tuple2(random.nextInt(size), random.nextInt(size))

  println(minesSet)

  def hit(x: Int, y: Int): java.util.Optional[Integer] =
    if minesSet.contains(Tuple2(x, y)) then
      OptionToOptional(None())
    else
      selected += Tuple2(x, y)
      OptionToOptional(Some(neighbours(x, y))) // Option => Optional converter

  def won: Boolean =
    selected.size + minesSet.size == size * size

  private def neighbours(x: Int, y: Int) =
    val intX: Stream[Int] = Stream.take(Stream.iterate(x - 1)(_ + 1))(LogicsImpl.Offset)
    val intList: List[Int] = toList(intX)
    val intY: Stream[Int] = Stream.take(Stream.iterate(y - 1)(_ + 1))(LogicsImpl.Offset)
    val pairList: List[(Int, Int)] = flatMap(intList)(x => toList(Stream.map(intY)(y => Tuple2(x, y))))
    val filteredList: List[(Int, Int)] = List.filter(pairList)(minesSet.toList.contains(_))
    List.length(filteredList)


