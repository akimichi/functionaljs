"use strict";

/*
 $ node --harmony bin/cat.js test/resources/dream.txt

*/

var pair = require('./pair');
// var pair = {
//   // pair の代数的データ構造
//   cons: (left, right) => {
//     return (pattern) => {
//       return pattern.cons(left, right);
//     };
//   },
//   match : (data, pattern) => {
//     return data(pattern);
//   },
//   // ペアの右側を取得する
//   right: (tuple) => {
//     return pair.match(tuple, {
//       cons: (left, right) => {
//         return right;
//       }
//     });
//   },
//   // ペアの左側を取得する
//   left: (tuple) => {
//     return pair.match(tuple, {
//       cons: (left, right) => {
//         return left;
//       }
//     });
//   }
// };

var IO = {
  /* #@range_begin(io_monad_definition_with_world) */
  // unit:: T => IO[T]
  unit: (any) => {
    return (world) =>  {  // 現在の外界
      return pair.cons(any, world);
    };
  },
  // flatMap:: IO[T] => (T => IO[S]) => IO[S]
  flatMap: (instanceA) => {
    return (actionAB) => { // actionAB:: a -> IO b
      return (world) => {
        var newPair = instanceA(world); // 現在の外界のなかで instanceAのIOアクションを実行する
        return pair.match(newPair,{
          cons: (value, newWorld) => {
            return actionAB(value)(newWorld); // 新しい外界のなかで、actionAB(value)で作られたIOアクションを実行する
          }
        });
      };
    };
  },
  /* #@range_end(io_monad_definition_with_world) */
  // flatMap: (instanceA) => {
  //   return (actionAB) => { // actionAB:: a -> IO b
  //     return IO.unit(IO.run(actionAB(IO.run(instanceA))));
  //   };
  // },
  /* #@range_begin(io_monad_definition_with_world_helper_function) */
  // done:: T => IO[T]
  done: (any) => {
    return IO.unit();
  },
  // run:: IO[A] => A
  run: (instance) => {
    return (world) => {
      return pair.left(instance(world)); // IOアクションを現在の外界に適用し、結果のみを返す
      // var newPair = instance(world); // ioモナドのインスタンス(アクション)を現在の外界に適用する
      // return pair.left(newPair);
    };
    // return instance();
  },
  /* #@range_end(io_monad_definition_with_world_helper_function) */
  /* #@range_begin(io_actions) */
  // readFile:: STRING => IO[STRING]
  readFile: (path) => {
    return (world) => {
      var fs = require('fs');
      var content = fs.readFileSync(path, 'utf8');
      return IO.unit(content)(world);
    };
  },
  // println:: STRING => IO[null]
  println: (message) => {
    return (world) => {
      console.log(message);
      return IO.unit(null)(world);
    };
  }
  /* #@range_end(io_actions) */
};


 
/* #@range_begin(io_monad_combined) */
var initialWorld = null;
var path = process.argv[2];

IO.flatMap(IO.readFile(path))((content) => {
  return IO.flatMap(IO.println(content))((_) => {
    return IO.done(_);
  });
})(initialWorld);
/* #@range_end(io_monad_combined) */
