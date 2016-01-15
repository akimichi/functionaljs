# 「関数型プログラミングの基礎知識」サンプルコード

## テスト環境の準備

### dockerを使う


~~~
docker build -t="functionaljs:v1" .
~~~


~~~
docker run -t="functionaljs:v1" -i -v `pwd`:/workspace . gulp test 
~~~

build -t="emile/nginx:v1" .

### node.js

nvm のインストール

node.js v0.12.0 のインストール

### scala

sbtのインストール

### haskell

cabal

stack

c.f. http://docs.haskellstack.org/en/stable/install_and_upgrade.html#ubuntu
sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 575159689BEFB442
sudo apt-get update && sudo apt-get install stack -y

stack build
stack setup
stack test

~~~
$ stack build
~~~

## テストの実行 

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
$ ./node_modules/.bin/gulp js-test
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

もしくは、 gulp を使って

~~~
$ gulp scala-test
~~~

### haskellのテスト

haskellのコードをテストする

~~~
$ cabal test
~~~

もしくは、 gulp を使って

~~~
$ gulp haskell-test
~~~

## プロジェクトページ


http://akimichi.github.io/functionaljs/

