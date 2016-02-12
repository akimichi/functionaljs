"use strict";

var expect = require('expect.js');
var pair = require('./pair');
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

module.exports = {
  /* #@range_begin(io_monad_definition_with_world) */
  // unit:: a -> IO a
  unit: (any) => {
    return (_) =>  {
      return any;
    };
  },
  /* #@range_begin(io_monad_definition_with_world_helper_function) */
  // done:: T -> IO T
  done: (any) => {
    var self = this;
    return self.unit();
  },
  // flatMap:: IO a -> (a -> IO b) -> IO b
  flatMap: (instanceA) => {
    var self = this;
    return (actionAB) => { // actionAB:: a -> IO b
      return self.unit(self.run(actionAB(self.run(instanceA))));
    };
  },
  // run:: IO A -> A
  run: (instance) => {
    return instance();
  },
  /* #@range_end(io_monad_definition_with_world_helper_function) */
  /* #@range_begin(io_actions) */
  // readFile:: STRING => IO[STRING]
  readFile: (path) => {
    return () => {
      var fs = require('fs');
      var content = fs.readFileSync(path, 'utf8');
      expect(content).to.a('string');
      return content;
    };
  },
  // println:: STRING => IO[null]
  println: (message) => {
    return () => {
      console.log(message);
      return null;
    };
  }
  /* #@range_end(io_actions) */
};
/* #@range_end(io_monad_definition_with_world) */

