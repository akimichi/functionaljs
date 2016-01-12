# 「関数型プログラミングの基礎知識」サンプルコード

## 準備

### node.js

### scala

sbt

### haskell

cabal


## 利用法

### node.js のテスト

ローカル環境に nvm がインストールされていることが必要です。

~~~
$ git clone git@github.com:akimichi/functionaljs.git
$ cd functionaljs
$ nvm use
$ npm install
~~~

テストを実行するには

~~~
$ ./node_modules/.bin/gulp test
~~~

ドキュメントを生成するには

~~~
$ ./node_modules/.bin/gulp doc
~~~

### scalaのテスト

scalaのコードをテストする

~~~
$ sbt test
~~~
~~~
$ gulp scala-test
~~~

### haskellのテスト

haskellのコードをテストする

~~~
$ cabal test
~~~

~~~
$ gulp haskell-test
~~~

## プロジェクトページ


http://akimichi.github.io/functionaljs/

