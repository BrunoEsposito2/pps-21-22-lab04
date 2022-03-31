package u04lab.polyglot.a01a
import Logics.*
import u04lab.polyglot.Pair

import scala.collection.immutable.HashSet
import scala.util.Random

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a01a/sol2/ */
object LogicsImpl:
  private val Failures = 5

class LogicsImpl(private val size: Int, private val boat: Int) extends Logics:

  private var hits: Set[(Int, Int)] = Set()

  private val random = Random()
  private val boatRow: Int = random.nextInt(size)
  private val boatLeftCol: Int = random.nextInt(size - boat + 1)
  private val boatSize: Int = size
  private var failures: Int = _

  println(toString)

  def hit(row: Int, col: Int) =
    if row == boatRow && col >= boatLeftCol && col < boatLeftCol + boatSize
    then
      hits + Tuple2(row, col)
      if (hits.size == boatSize) Result.WON else Result.HIT
    else
      failures += 1
      if (failures == LogicsImpl.Failures) Result.LOST else Result.MISS

  override def toString =
    "x = " + boatLeftCol + " y = " + boatRow

