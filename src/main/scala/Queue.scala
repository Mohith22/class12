class Queue[+T] private (private[this] val queue: List[T]) {
  
  def enqueue[U >: T](x: U) = {
    new Queue[U](queue :+ x)
  }

  def head: T = {
    require(!isEmpty, "Queue.head on empty queue")
    queue.last
  }
  
  def dequeue: (T, Queue[T]) = {
    require(!isEmpty, "Queue.dequeue on empty queue")
    val x :: queue1 = queue
    (x, new Queue(queue1))
  }

  def isEmpty: Boolean = queue.isEmpty

  override def toString: String = {
    s"Queue${queue.toString.drop(4)}"
  }
}

object Queue {
  def empty[T]: Queue[T] = new Queue(Nil)

  def apply[T](xs: T*): Queue[T] = new Queue(xs.toList)
}

object QueueTest extends App {
  val q: Queue[Int] = Queue.empty
  
  val q1 = q.enqueue(1).enqueue(2).enqueue(3).enqueue(4)
  
  val qAny: Queue[Any] = q1
  
  println(qAny.enqueue("Hello"))
  
  println(q1.dequeue._2.dequeue)
  println(q1)
}