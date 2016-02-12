"use strict";

/*
 $ node --harmony bin/cat.js test/resources/dream.txt

*/

var pair = require('./pair');

/* #@range_begin(io_monad_definition) */
var IO = {
  // unit:: T => IO[T]
  unit: (any) => {
    return (_) =>  { // 外界を指定しない
      return any;  // 値だけを返す
    };
  },
  // flatMap:: IO[T] => FUN[T => IO[S]] => IO[S]
  flatMap: (instanceA) => {
    return (actionAB) => { // actionAB:: a -> IO b
      return actionAB(IO.run(instanceA)); // instanceAのIOアクションを実行し、続いて actionABを実行する
    };
  },
  // done:: T => IO[T]
  done: (any) => {
    return IO.unit();
  },
  // run:: IO[A] => A
  run: (instance) => {
    return instance();
  },
  // readFile:: STRING => IO[STRING]
  readFile: (path) => {
    var fs = require('fs');
    return IO.unit(fs.readFileSync(path, 'utf8'));
  },
  // println:: STRING => IO[]
  println: (message) => {
    console.log(message);
    return IO.unit();
  }
};
/* #@range_end(io_monad_definition) */

 
/* #@range_begin(io_monad_combined) */
var path = process.argv[2];

IO.flatMap(IO.readFile(path))((content) => {
  return IO.flatMap(IO.println(content))((_) => {
    return IO.done(_);
  });
})(); // 外界を明示的に渡さず、単に関数を実行するだけ
/* #@range_end(io_monad_combined) */
