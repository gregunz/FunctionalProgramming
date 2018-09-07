package interpreter

object Main extends App {
  import java.io.{BufferedReader, InputStreamReader}
  val in = new BufferedReader(new InputStreamReader(System.in))
  // TODO: Insert code for the REPL
  while(1>0){
    print("lisp>")
    val input = in.readLine()
    try{
      println(Lisp.lisp2string( Lisp.evaluate(input) ))
    }catch{
      case e:Exception =>
        println(e)
    }
  }

}

object LispCode {
  // TODO: implement the function `reverse` in Lisp.
  // From a list (a, b, c, d) it should compute (d, c, b, a)
  // Write it as a String, and test it in your REPL
  /* (def (reverse L) (car (cdr L)) (reverse (cons 1 (cons 2 (cons 3 nil)))))

  def (loop L prev acc) ( if (null? L) acc  (loop (cdr L) (car L) (cons (- (car L) prev) acc)) ) (def (differences L2) (loop L2 0 nil))
   */
  // (cons 1 (cons 2 (cons 3 nil)))
  val reverse = """
    def (reverse L acc) (if (null? L) acc (reverse (cdr L) (cons (car L) acc)) )
  """

  // TODO: implement the function `differences` in Lisp.
  // From a list (a, b, c, d ...) it should compute (a, b-a, c-b, d-c ...)
  // You might find useful to define an inner loop def
  val differences = """
  def (differences L2)(def (loop L prev acc)(if (null? L)acc( loop (cdr L) (car L) (cons (- (car L) prev) acc) ))(reverse (loop L2 0 nil) nil))
  """
  val rebuildList = """
  def (rebuildList L) (def (loop L2 prev acc) (if (null? L2) acc (loop (cdr L2) (+ prev (car L2)) (cons (+ prev (car L2)) acc))) (reverse (loop L 0 nil) nil ) )
  """

  val withDifferences: String => String =
    (code: String) => "(" + reverse + " (" + differences + " (" + rebuildList + " " + code + ")))"
}
