import java.util.Scanner

object p3 extends App {
  val op = Set('*', '-', '+')
  val std: Scanner = new Scanner(System.in)
  val exp = std.nextLine()
  val x = std.nextLine() match {
    case "Odd" => '1'
    case "Even" => '2'
  }
  val y = std.nextLine() match {
    case "Odd" => '1'
    case "Even" => '2'
  }
  var cs = exp.toList
  var ops = Vector[Char]()
  cs = cs.map{case 'x' => x; case 'y' => y; case c => c}

  def simplify(cs :List[Char]): List[Int] = {
    cs match {

      case c :: Nil => (c.toInt % 2) :: Nil
      case c1 :: c2 :: css  =>
        if (op.contains(c2)) {
          ops = ops :+ c2
          (c1.toInt % 2) :: simplify(css)
        }else
          simplify(cs.tail)
    }
  }
  def eval(exp: List[Int], op: List[Char]): Int = {
    (exp, op) match {
      case (e :: exps, o :: Nil) =>
        if (o == '*')
          e * exps.head
        else
          e + exps.head

      case (e :: exps, o :: ops) =>
        if (o == '*')
          e * eval(exps, ops)
        else
          e + eval(exps, ops)
    }
  }

  val simplified = simplify(cs)
  val result = eval(simplified, ops.toList)
  val isEven = result%2 == 0

  println(if(isEven) "Even" else "Odd")
}