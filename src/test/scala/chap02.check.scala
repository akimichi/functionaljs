import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop.{forAll, BooleanOperators}
import scala.annotation.tailrec

object MathSpecification extends Properties("Math") {
  def factorial(i: Int): Long = {
    @tailrec
    def fact(i: Int, accumulator: Int): Long = {
      if (i <= 1) accumulator
      else fact(i - 1, i * accumulator)
    }

    fact(i, 1)
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
  property("multiply") = forAll( (x:Int) => 
    x+x == 2*x 
  )
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
