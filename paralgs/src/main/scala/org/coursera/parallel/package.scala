package org.coursera

package object parallel {
  def parallel[A, B](taskA: => A, taskB: => B): (A, B) = {
    var resultA: Option[A] = None
    var resultB: Option[B] = None

    val threadA = new Thread(new Runnable {
      override def run() = resultA = Some(taskA)
    })

    val threadB = new Thread(new Runnable {
      override def run() = resultB = Some(taskB)
    })

    threadA.start()
    threadB.start()
    threadA.join()
    threadB.join()

    (resultA.get, resultB.get)
  }
}
