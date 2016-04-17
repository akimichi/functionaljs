import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop.{forAll, BooleanOperators}
import scala.annotation.tailrec

object MathSpecification extends Properties("Math") {
  def factorial(n: Int): Long = {
    @tailrec
    def fact(n: Int, accumulator: Int): Long = {
      if (n <= 1) accumulator
      else fact(n - 1, n * accumulator)
    }
    fact(n, 1)
  }
  // def factorial(n: Int): Int = {
  //   @tailrec def factorialAcc(acc: Int, n: Int): Int = {
  //     if (n <= 1) acc
  //     else factorialAcc(n * acc, n - 1)
  //   }
  //   factorialAcc(1, n)
  // }
  def perm(n:Int, r:Int) = {
    factorial(n) / factorial(n-r)
  }
  def comb(n:Int, r:Int) = {
    perm(n,r) / factorial(r)
  }
  // property("comb") = forAll { (n: Int, r: Int) =>
  //   (n > 1 && r > 1 && n > r)  ==>   (comb(n,r) == comb(n, n-r))
  // }
  def succ(n:Int) = {
    n + 1
  }
  /* #@range_begin(check_succ) */
  property("succ") = forAll( (x:Int) => 
    succ(0) + succ(x) == succ(succ(x))
  )
  /* #@range_end(check_succ) */
  /* #@range_begin(check_multiplication) */
  property("multiply") = forAll( (x:Int) => 
    x+x == 2*x 
  )
  /* #@range_end(check_multiplication) */
}

object StringSpecification extends Properties("String") {

  property("startsWith") = forAll { (a: String, b: String) =>
    (a+b).startsWith(a)
  }

  property("concatenate") = forAll { (a: String, b: String) =>
    (a+b).length >= a.length && (a+b).length >= b.length
  }

  property("substring") = forAll { (a: String, b: String, c: String) =>
    (a+b+c).substring(a.length, a.length+b.length) == b
  }

}
