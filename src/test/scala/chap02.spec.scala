import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.{ FunSpec, BeforeAndAfterAll, BeforeAndAfterEach, BeforeAndAfter }


class Chap02Spec extends FunSpec with ShouldMatchers {
  describe("テストが容易である"){
    describe("銀行口座の例"){
      // ##@range_begin(account_class)
      class Account(amount: Int) {    // 銀行口座のクラス
        var balance = amount          // 口座の残高を可変な状態として作る
        def deposit(amount: Int):Int = {  // 口座にお金を預ける
          balance = balance + amount      // 残高を更新する
          return balance
        }
        def withdraw(amount: Int):Int = { // 口座からお金を引き出す
          balance = balance - amount      // 残高を更新する
          return balance
        }
      }
      // ##@range_end(account_class)
      it("取り引きの例"){
        /************* テスト *************/
        // ##@range_begin(account_with_state_test)
        val theAccount = new Account(1000) // 口座を1000円で開設する
        theAccount.balance should equal(1000)
        theAccount.withdraw(200)           // 口座から200円を引き出す
        theAccount.balance should equal(800) // 残高は800円となる
        // ##@range_end(account_with_state_test)
      }
      it("貧乏人の例"){
        /************* テスト *************/
        // ##@range_begin(poormansAccount_test)
        val poormansAccount = new Account(1000) // 口座を1000円で開設する
        poormansAccount.balance should equal(1000)
        poormansAccount.withdraw(200)           // 口座から200円を引き出す
        poormansAccount.balance should equal(800) // 残高は800円となる
        // ##@range_end(poormansAccount_test)
      }
      it("金持ちの例"){
        /************* テスト *************/
        // ##@range_begin(richmansAccount_test)
        val richmansAccount = new Account(100000) // 口座を100万円で開設する
        richmansAccount.balance should equal(100000)
        richmansAccount.withdraw(20000)           // 口座から20万円を引き出す
        richmansAccount.balance should equal(80000) // 残高は80円となる
        // ##@range_end(richmansAccount_test)
      }
    }
  }
}
