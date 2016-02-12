"use strict";

var pair = require('./pair');

/* #@range_begin(pair_datatype) */
// var pair = {
//   // pair の代数的データ構造
//   cons: (left, right) => {
//     return (pattern) => {
//       return pattern.cons(left, right);
//     };
//   },
//   match : (data, pattern) => {
//     return data.call(pair, pattern);
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
/* #@range_end(pair_datatype) */

var IO = {
  /* #@range_begin(io_monad_definition_with_world) */
  // unit:: a -> IO a
  unit: (any) => {
    return (world) =>  {
      return any;
    };
  },
  // flatMap:: IO a -> (a -> IO b) -> IO b
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
  /* #@range_begin(io_monad_definition_with_world_helper_function) */
  // done:: T -> IO T
  done: (any) => {
    return this.unit();
  },
  // run:: IO A -> A
  run: (instance) => {
    return instance();
  },
  /* #@range_end(io_monad_definition_with_world_helper_function) */
  /* #@range_begin(io_actions) */
  // readFile:: STRING => IO[STRING]
  readFile: (path) => {
    return (io) => {
      var fs = require('fs');
      var content = fs.readFileSync(path, 'utf8');
      return content;
    };
  },
  // println:: STRING => IO[null]
  println: (message) => {
    return (io) => {
      console.log(message);
      return null;
    };
  }
  /* #@range_end(io_actions) */
};
/* #@range_end(io_monad_definition_with_world) */


