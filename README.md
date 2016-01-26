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

node.jsのコードをテストする。

~~~
docker run -it --rm --workdir="/workspace/nodejs" ric/functionaljs:v1 /bin/bash --login -i -c "gulp --harmony js-test"
~~~

scala のコードをテストする

~~~
docker run -it --rm --workdir="/workspace/scala" ric/functionaljs:v1 /bin/bash -c "sbt test"
~~~

haskell のコードをテストする

~~~
docker run -it --rm  --workdir="/workspace/haskell" ric/functionaljs:v1 /bin/bash -c "stack test"
~~~

コンテナにログインする

~~~
docker run -it  --workdir="/workspace" ric/functionaljs:v1 bash --login -i
root@ca7528c7f2e5:/workspace# cd nodejs && gulp test 
root@ca7528c7f2e5:/workspace# mocha --harmony test
root@ca7528c7f2e5:/workspace# cd scala && sbt test
root@ca7528c7f2e5:/workspace# cd haskell && stack test 
~~~


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

