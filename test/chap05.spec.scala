trait Currency
case class Yen(amount: Int) extends Currency
case class Dollar(amount: Int) extends Currency

def toS(currency:Currency):String = currency match { // 通貨の型でマッチさせる
  case Yen(amount) => {
    "%s yen".format(amount) // 変数amountには日本円の値が入る
  }
  case Dollar(amount) => {
    "%s dollar".format(amount)  // 変数amountにはドルの値が入る
  }
}
val yen = Yen(1000)
toS(yen) should equal("1000 yen")
