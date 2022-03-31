package u04lab.polyglot.a05b

import u04lab.polyglot.Pair

import scala.util.Random

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a05b/sol2/ */
class LogicsImpl(private val size: Int) extends Logics:

  private val gridSize: Int = size
  private val random = Random()
  private val initial = (random.nextInt(size-2)+1, random.nextInt(size-2)+1)
  private var tickCount: Int = 0

  override def tick(): Unit = tickCount += 1

  override def isOver: Boolean =
    initial._2 - tickCount < 0 || initial._2 + tickCount >= gridSize
    || initial._1 - tickCount < 0 || initial._1 + tickCount >= gridSize

  override def hasElement(x: Int, y: Int): Boolean =
    (x == initial._1 && Math.abs(y - initial._2) <= tickCount) ||
      (y == initial._2 && Math.abs(x - initial._1) <= tickCount) ||
      (x - y == initial._1 - initial._2 && Math.abs(x - initial._1) <= tickCount) ||
      (x + y == initial._1 + initial._2 && Math.abs(x - initial._1) <= tickCount)
