# 正誤表

読者の方から、メールやGitHub Issueを介してコードの不具合の報告を受けました。
ここでは、指摘された不具合についての訂正と解説をします。

なお書籍の正式な正誤表は、[リックテレコム社のサイト](http://www.ric.co.jp/book/error/error1059.html)にあります。
同サイトと比べて本ページは、より技術的な詳細を記したものです。

---

## 問題-01 「クロージャで不変なデータ構造を作る」におけるデータの初期化


### 問題の所在

7.3章「クロージャで不変なデータ構造を作る」のリスト7.32「カリー化された不変なオブジェクト型のテスト」(220頁)では、不変なオブジェクトを初期化する際に`object.empty()`を渡しています。

```js
var obj = object.set("C3PO", "Star Wars")(object.empty());
                                          ^^^^^^^^^^^^^^
```

ところがこのコードには不具合があります。
なぜならば、以下のように存在しないキー(下記の例では"鉄腕アトム")を指定するとエラーになるからです。

```js
object.get("鉄腕アトム")(obj)
```

簡約の過程を追跡すると、エラーの源がわかります。


```js
object.get("鉄腕アトム")(obj)
=>
object.get("鉄腕アトム")(
  object.set("C3PO", "Star Wars")(object.empty())
)
=>
object.get("鉄腕アトム")(
  (queryKey) => {
         if("C3PO" === queryKey) {
           return "Star Wars";
         } else {
           return object.get(queryKey)(object.empty());
         }
       }
)
=>
object.get("鉄腕アトム")(
  (queryKey) => {
         if("C3PO" === queryKey) {
           return "Star Wars";
         } else {
           return object.empty()(queryKey)
         }
       }
)
=>
((queryKey) => {
       if("C3PO" === queryKey) {
         return "Star Wars";
       } else {
         return object.empty()(queryKey)
       }
     }
)("鉄腕アトム")
=>
if("C3PO" === "鉄腕アトム") {
  return "Star Wars";
} else {
  return object.empty()("鉄腕アトム")
}
=>
  return object.empty()("鉄腕アトム")

=>
  return null("鉄腕アトム")
=>
  ここでnullを評価しようとしてエラーとなる    
```

つまり、存在しないキーで検索すると、最後にnullを評価しようとしてエラーになるのです。

### 解決策

この問題を解決するには、不変なオブジェクトを初期化する際に`object.empty()`ではなく、**object.emptyを渡す**のが正解です。

```js
// object.setの場合
var obj = object.set("C3PO", "Star Wars")(object.empty);
                                          ^^^^^^^^^^^^
// composeでobject.setを合成する場合
var robots = compose( // object.setを合成する
  object.set("C3PO", "Star Wars"), 
  object.set("HAL9000","2001: a space odessay") 
)(object.empty);
  ^^^^^^^^^^^^
```

これがなぜ正しいかは、objectモジュールの各関数についてその型を考察すると明らかです。
以下に、不変なオブジェクト型を実現したobjectモジュールのコードを再掲します。

```js
var object = {  // objectモジュール
  empty: () => {
    return null;
  },
  // (STRING,Any) => FUN[STRING => Any] => STRING => Any
  set: (key, value) => {
    return (obj) => {
      return (queryKey) => {
        if(key === queryKey) {
          return value;
        } else {
          return object.get(queryKey)(obj);
        }
      };
    };
  },
  // (STRING) => FUN[STRING => Any] => Any
  get: (key) => {
    return (obj) => {
      return obj(key);
    };
  }
// 以下、省略
```

`object.set`関数は、`(STRING,Any) => FUN[STRING => Any] => STRING => Any`の型を持つことになります。
同様にして`object.get`関数は、 `(STRING) => FUN[STRING => Any] => Any` の型を持ちます。
なお、Anyは全ての型の基底型を表現するものとします。

さて問題となっているコードは、以下のように、`object.set`関数を用いて不変なオブジェクト型を構築しようとしています。


```js
var obj = object.set("C3PO", "Star Wars")(???);
```

ここで`object.set`関数の第1引数の`(STRING,Any)`には`("C3PO", "Star Wars")`が渡されています。
これは問題ありません。
ところ第2引数には`FUN[STRING => Any]`の型をもつ関数を実引数として渡さなければならないのに、実引数として渡されているのは`object.empty()`です。
`object.empty()`はnullという値なので、本来渡すべき`FUN[STRING => Any]`という型と合致しません。

結局、正しく `FUN[STRING => Any]` の関数を渡すには、???は`object.empty()`ではなく、`object.empty`でなければならないのです。
さらにこれを明示的に表現するには `object.empty`関数の定義を以下のようにしたほうがより正確です。

```js
// empty:: FUN[STRING => Any]
empty: (key) => {
  return null;
},
```

以上を前提とすれば、コードの全貌は以下のようになります。

```js
var object = {  // objectモジュール
   // empty:: STRING => Any
   empty: (key) => {
     return null;
   },
   // set:: (STRING,Any) => FUN[STRING => Any] => STRING => Any
   set: (key, value) => {
     return (obj) => {
       return (queryKey) => {
         if(key === queryKey) {
           return value;
         } else {
           return object.get(queryKey)(obj);
         }
       };
     };
   },
   // get:: (STRING) => FUN[STRING => Any] => Any
   get: (key) => {
     return (obj) => {
         return obj(key);
       };
     }
   };

   // **リスト7.32** カリー化された不変なオブジェクト型のテスト
   var robots = compose( // object.setを合成する
     object.set("C3PO", "Star Wars"), 
     object.set("HAL9000","2001: a space odessay") 
   )(object.empty);
   // )(object.empty());

   expect(
     object.get("HAL9000")(robots)
   ).to.eql(
     "2001: a space odessay"
   );
   expect(
     object.get("C3PO")(robots)
   ).to.eql(
     "Star Wars"
   );
   // 該当するデータがなければ、nullが返る
   expect(
     object.get("鉄腕アトム")(robots)
   ).to.eql(
     null
   );
}
```

今回の不具合から以下の教訓が得られました。

* 型の整合性をきちんと確認する
* 単体テストでいろいろなケース(特にコーナーケース)を網羅する


## 問題-02 「IOモナドで副作用を閉じこめる」におけるIOモナドの定義

この問題は[Issue #1](https://github.com/akimichi/functionaljs/issues/1) で取りあげられ、書籍では284頁から286頁に関係します。
解決策は、[tanagumoさん](https://github.com/tanagumo)に提供していただきました。


### 問題の所在

'a'を出力するIOアクションと'b'を出力するIOアクションをIO.flatMapで合成してみます。
本来ならばIOアクションを合成したものはIOアクションであり、それは`IO.run`関数で実行するまでは遅延されるはずです。
つまり`IO.run` によってIOモナドから値が取り出されてはじめて、副作用が実行されなければなりません。
ところが以下のコードでわかるように、`IO.run(ab)`の実行前(つまりIOアクションabが定義された時点)ですでに`ab`が表示されてしまっているのです。

```js
var ab = IO.flatMap(IO.putChar('a'))((_) =>
    IO.flatMap(IO.putChar('b'))((_) =>
      IO.done()
    )
  );
abundefined // この時点で副作用（画面表示)が発生して、'ab'が出力されてしまう
> IO.run(ab);
undefined   // 本来はこの時点で表示されるべきなのに、IO.runの時点では副作用は発生しない。
```

このように書籍に掲載したIOモナドの定義には、副作用の実行タイミングに問題があるのです。
そこで`IO.flatMap`関数と`IO.putChar`関数の定義を振り返ってみます。

`IO.flatMap`関数は、リスト7.99「外界を明示しないIOモナドの定義」(284頁)で以下のように定義されています。

```js
/* flatMap:: IO[T] => FUN[T => IO[U]] => IO[U] */
flatMap : (instanceA) => {
  return (actionAB) => { // actionAB:: a -> IO[b]
    return IO.unit(IO.run(actionAB(IO.run(instanceA))));
  };
},
```

この定義では、`IO.flatMap(instanceA)(actionAB)`の結果は`actionAB(IO.run(instanceA))`の評価結果になります。
JavaScriptは実引数を正格評価するため、`IO.run(instanceA)`が評価されて、この時点で副作用が発生してしまうのです。

また、`IO.putChar`関数は、リスト7.101(286頁)にて次のように定義されています。

```js
/* putChar:: CHAR => IO[Unit] */
putChar: (character) => {
  /* 1文字だけ画面に出力する */
  process.stdout.write(character); 
  return IO.unit(null);
},
```

`IO[Unit]`の型は`T => IO[T]`のため、上記の`putChar`の定義は型レベルでは`CHAR => IO[Unit]`となっています。
しかし、上記の実装では`putChar`を文字に適用した時点で`process.stdout.write(character)`が実行されてしまいます。

### 解決策

問題の本質は、`flatMap`、`putChar`共にIOアクションを実行するタイミングが早すぎることでした。
書籍にも説明したとおり、高階関数を活用することで実行のタイミングを遅らせることができます。
そこで、無名関数を利用してIO.flatMap関数を以下のように定義しなおしてみます。

```js
// flatMap :: IO[A] => FUN[A => IO[B]] => IO[B]
flatMap : (instanceA) => {
  return (actionAB) => { // actionAB:: A -> IO[B]
    return (_) => { // 無名関数
      return IO.run(actionAB(IO.run(instanceA)));
    }
  };
},
```

この定義では`IO.flatMap(instanceA)(actionAB)`としたとき、`(_) => {...}`の無名関数の中身はまだ実行されません。
そして`IO.run(actionAB(IO.run(instanceA)))`としてIOアクションを実行すると初めて、この無名関数が評価されることになります。

`IO.putChar`関数も同様に下記の通り訂正する必要があります。

```js
// putChar :: CHAR => IO[Unit]
putChar: (character) => {
  return (_) => {
    process.stdout.write(character);
    return null;
  };
},
```

最後に、上記の要請を満たすIOモナドの定義(必要箇所のみ)を下記に掲載します。

```js
// type IO[A] = FUN[() => A]
var IO = { 
  // unit :: A => IO[A]
  unit: (any) => {
    return () =>  
      return any;
  },
  // run :: IO[A] => A
  run: (instance) => {
    return instance();
  },
  // flatMap :: IO[A] => FUN[A => IO[B]] => IO[B]
  flatMap : (instanceA) => {
    return (actionAB) => { // actionAB:: A -> IO[B]
      return (_) => {
        return IO.run(actionAB(IO.run(instanceA)));
      }
    };
  },
  // putChar :: CHAR => IO[Unit]
  putChar: (character) => {
    return (_) => {
      process.stdout.write(character);
      return null;
    };
  }
};
```

今回の不具合で、以下の教訓が得られました。

* 型は実行の順番までは保証しない
* 副作用は実行の順番が重要である

## 問題-03 Windows10で動かす



~~~
$ nvm install v0.12.0
~~~


### WSLを使う

- [Windows 10でLinuxプログラムを利用可能にするWSLをインストールする（バージョン1803以降対応版）](https://www.atmarkit.co.jp/ait/articles/1608/08/news039.html)


WSLを起動

日本語環境をインストール

- https://www.atmarkit.co.jp/ait/articles/1806/28/news043.html



~~~
git clone ...

# nvmをインストール
curl -o- https://raw.githubusercontent.com/creationix/nvm/v0.31.1/install.sh | bash
source ~/.bashrc
nvm install v0.12.0
nvm use
npm install -g mocha@1.21.5
npm install -g gulp@3.9.1
npm install
#  おおくのワーニング

~~~


いずれも失敗する。
~~~
mocha --harmony
あるいは
gulp js-test はシッパイする
~~~






