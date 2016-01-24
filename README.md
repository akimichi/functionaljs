# 「関数型プログラミングの基礎知識」サンプルコード

このレポジトリは、リック・テレコム社刊行の「関数型プログラミングの基礎知識」のサンプルコードをおさめたものです。

## 利用法 

~~~
$ git clone git@github.com:akimichi/functionaljs.git
~~~

テスト環境の構築には、 1) docker を使う 2) 個別にインストールする、 の2つの方法があります。
dockerを利用したほうが確実です。


### dockerを使う

まず最初にdockerイメージを生成します。

~~~
docker build -t="ric/functionaljs:v1" .
~~~

テストを実行する

~~~
docker run -it --rm -v `pwd`:/workspace ric/functionaljs:v1 /bin/bash --login -i -c "gulp --harmony js-test"
# docker run -it --rm -v `pwd`:/workspace ric/functionaljs:v1 gulp --harmony js-test
# docker run -it --rm -v `pwd`:/workspace ric/functionaljs:v1 mocha --harmony test 
~~~

scala のコードをテストする

~~
docker run -it --rm -v `pwd`:/workspace ric/functionaljs:v1 /bin/bash -c "sbt test"
# docker run -it --rm -v `pwd`:/workspace ric/functionaljs:v1 sbt test
~~~

haskell のコードをテストする

~~
docker run -it --rm -v `pwd`:/workspace ric/functionaljs:v1 /bin/bash -c "stack build && stack test"
# docker run -it --rm -v `pwd`:/workspace ric/functionaljs:v1 /bin/bash -c "stack test"
~~~

コンテナにログインする

~~~
docker run -it  -v `pwd`:/workspace ric/functionaljs:v1 bash --login -i
root@ca7528c7f2e5:/workspace# gulp test 
root@ca7528c7f2e5:/workspace# mocha --harmony test
root@ca7528c7f2e5:/workspace# sbt test
~~~


<!-- ~~~ -->
<!-- docker exec -i -t ric/functionaljs:v1 bash -->
<!-- ~~~ -->


### ローカル環境にテスト環境を個別にインストールする

#### node.js

nvm のインストール

node.js v0.12.0 のインストール

#### scala

sbtのインストール

#### haskell

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

### テストの実行 

#### node.js のテスト

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

#### scalaのテスト

scalaのコードをテストする

~~~
$ sbt test
~~~

もしくは、 gulp を使って

~~~
$ gulp scala-test
~~~

#### haskellのテスト

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

