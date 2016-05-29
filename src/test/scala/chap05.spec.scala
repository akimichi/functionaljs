import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.{ FunSpec, BeforeAndAfterAll, BeforeAndAfterEach, BeforeAndAfter }


class Chap05Spec extends FunSpec with ShouldMatchers {
  describe("ifについて"){
    it("even"){
      // ##@range_begin(if_as_expression_in_scala)
      def even(number:Int): Boolean = {
        return if(number % 2 == 0) // ifは式なのでその結果を返す
          true
        else
          false
      }
      /************* テスト *************/
      even(2) should equal(true)
      even(3) should equal(false)
      // ##@range_end(if_as_expression_in_scala)
    }
  }
  describe("switch文"){
    it("通貨の例"){
      // ##@range_begin(pattern_match_in_scala)
      trait Currency
      case class Yen(amount: Int) extends Currency
      case class Dollar(amount: Int) extends Currency

      def toS(currency:Currency):String = 
        // 通貨の型でマッチさせる
        currency match { 
          // Yenにマッチする場合
          case Yen(amount) => { // 変数amountには日本円の値が入る
            "%s yen".format(amount) 
          }
          // Dollarにマッチする場合
          case Dollar(amount) => { // 変数amountにはドルの値が入る
            "%s dollar".format(amount)  
          }
        }
      /************* テスト *************/
      val yen = Yen(1000)
      toS(yen) should equal("1000 yen")
      // ##@range_end(pattern_match_in_scala)
    }
  }
}

