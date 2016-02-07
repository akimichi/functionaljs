# 「関数型プログラミングの基礎知識」サンプルコード

このレポジトリは、リック・テレコム社刊行の「関数型プログラミングの基礎知識」のサンプルコードをおさめたものです。

## 利用法 

まず最初に本レポジトリをクローンして、そのディレクトリに入ります。

~~~
$ git clone git@github.com:akimichi/functionaljs.git
$ cd functionaljs
~~~

テスト環境の構築には、 1) docker を使う 2) 個別にインストールする、 の2つの方法があります。
dockerを利用したほうが確実です。


### dockerを使う

まず最初に下記のコマンドでdockerイメージを生成します。
末尾のピリオド(.) も忘れずに入力してください。

~~~
docker build -t="ric/functionaljs:v1" .
~~~

### 単体テストを実行する

テストを実行するには、作成されたイメージをもとに dockerコンテナを起動します。

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

### REPLを実行する 

コンテナにログインする

~~~
docker run -it  --workdir="/workspace" ric/functionaljs:v1 bash --login -i
~~~

node.jsのREPLを試す。

~~~
cd nodejs/
node

> 1 + 2
3
> 
(^C again to quit)
~~~

scala のREPLを試す。

~~~
cd scala
sbt console

[info] Loading project definition from /workspace/scala/project
[info] Set current project to Functional Book Test Project (in build file:/workspace/scala/)
[info] Starting scala interpreter...
[info] 
Welcome to Scala version 2.9.1.final (Java HotSpot(TM) 64-Bit Server VM, Java 1.7.0_80).
Type in expressions to have them evaluated.
Type :help for more information.

scala> 1 + 2
res0: Int = 3

scala> :q
~~~

haskellのREPLを試す。

~~~
cd haskell
ghci

GHCi, version 7.10.2: http://www.haskell.org/ghc/  :? for help
Prelude> 1 + 2
3
Prelude> :q
Leaving GHCi.
~~~

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

以下のようなエラーでコンパイルが失敗する場合があります。

~~~
[error] error while loading CharSequence, class file '/usr/lib/jvm/java-8-oracle/jre/lib/rt.jar(java/lang/CharSequence.class)' is broken
~~~

そのときは、旧いバージョンのjavaを使ってください。
例えば ubuntu では、以下のようにして使用するjavaのバージョンを切りかえます。

~~~
sudo update-alternatives --config java
~~~


もしくは、 gulp を使って

~~~
$ gulp scala-test
~~~

#### haskellのテスト

haskellのコードをテストする

~~~
$ stack test
~~~

もしくは、 gulp を使って

~~~
$ gulp haskell-test
~~~

## プロジェクトページ


http://akimichi.github.io/functionaljs/

