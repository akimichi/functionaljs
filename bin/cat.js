"use strict";

/*
 $ node --harmony bin/cat.js test/resources/dream.txt

*/

var pair = require('./pair');

var IO = {
  /* #@range_begin(io_monad_definition_with_world) */
  // unit:: T => IO[T]
  unit: (any) => {
    return (world) =>  {  // 引数worldは現在の外界
      return pair.cons(any, world);
    };
  },
  // flatMap:: IO[A] => (A => IO[B]) => IO[B]
  flatMap: (instanceA) => {
    return (actionAB) => { // actionAB:: A -> IO[B]
      return (world) => {  // 引数worldは現在の外界
        /* 現在の外界のなかで instanceAのIOアクションを実行する */
        var newPair = instanceA(world); 
        return pair.match(newPair,{
          cons: (value, newWorld) => {
            /* 
               新しい外界のなかで、actionAB(value)で作られた
               IOアクションを実行する
            */
            return actionAB(value)(newWorld); 
          }
        });
      };
    };
  },
  /* #@range_end(io_monad_definition_with_world) */
  /* #@range_begin(io_monad_definition_with_world_helper_function) */
  /* done:: T => IO[T]
     done関数 */
  done: (any) => {
    return IO.unit();
  },
  /* run:: IO[A] => A */
  run: (instanceM) => {
    return (world) => {
      /* IOアクションを現在の外界に適用し、結果のみを返す */
      return pair.left(instanceM(world)); 
    };
  },
  /* #@range_end(io_monad_definition_with_world_helper_function) */
  /* #@range_begin(io_actions) */
  /* readFile:: STRING => IO[STRING] 
     readFile関数は、pathで指定されたファイルを
     読み込むIOアクション */
  readFile: (path) => {
    return (world) => { // 外界を引数とする 
      var fs = require('fs');
      var content = fs.readFileSync(path, 'utf8');
      return IO.unit(content)(world); // 外界を渡してIOアクションを返す
    };
  },
  /* println:: STRING => IO[]
     println関数は、messageで指定された文字列を
     コンソール画面に出力するIOアクション */
  println: (message) => {
    return (world) => { // IOモナドを返す
      console.log(message);
      return IO.unit(null)(world);
    };
  }
  /* #@range_end(io_actions) */
};


 
/* #@range_begin(io_monad_combined) */
var initialWorld = null;
var path = process.argv[2];

IO.flatMap(IO.readFile(path))((content) => { // contentにはファイルの内容が入っている
  return IO.flatMap(IO.println(content))((_) => {
    return IO.done(_);
  });
})(initialWorld);
/* #@range_end(io_monad_combined) */
