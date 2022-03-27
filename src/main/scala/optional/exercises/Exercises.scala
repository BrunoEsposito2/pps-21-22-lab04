package optional.exercises

import u04lab.code.*

object Test extends App:
  import u04lab.code.{Course, *}

  val c1 = u04lab.code.Course("PPS", "Viroli")
  val c2 = u04lab.code.Course("PCD", "Ricci")
  val c3 = u04lab.code.Course("SDR", "D'Angelo")
  val courses = List(c1, c2, c3)
  println(courses)

  import u04lab.code.Student.*

  courses match
    case sameTeacher(l) => println(s"$courses have same teacher $l")
    case _ => println(s"$courses have different teachers")