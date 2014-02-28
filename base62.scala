
object Base62 {

  val baseString: String = ( ('a' to 'z') ++ ('A' to 'Z') ++ (0 to 9)).mkString
  val base = 62

  def decode(s: String): Long = {
    s.zip(s.indices.reverse)
      .map { case (c, p) => baseString.indexOf(c) * scala.math.pow(base, p).toLong }
      .sum
  }

  def encode(i: Long): String = {

    @scala.annotation.tailrec
    def div(i: Long, res: List[Int] = Nil): List[Int] = {
      (i / base) match {
        case q if q > 0 => div(q, (i % base).toInt :: res)
        case _ => i.toInt :: res
      }
    }

    div(i).map(x => baseString(x)).mkString
  }

}
