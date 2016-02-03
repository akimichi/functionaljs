"use strict";

/* #@range_begin(pair_datatype) */
var pair = {
  // pair の代数的データ構造
  cons: (left, right) => {
    return (pattern) => {
      return pattern.cons(left, right);
    };
  },
  match : (data, pattern) => {
    return data.call(pair, pattern);
  },
  // ペアの右側を取得する
  right: (tuple) => {
    return pair.match(tuple, {
      cons: (left, right) => {
        return right;
      }
    });
  },
  // ペアの左側を取得する
  left: (tuple) => {
    return pair.match(tuple, {
      cons: (left, right) => {
        return left;
      }
    });
  }
};
/* #@range_end(pair_datatype) */

module.exports = {
// var io = {
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
    return this.unit();
  },
  // flatMap:: IO a -> (a -> IO b) -> IO b
  flatMap: (instanceA) => {
    return (actionAB) => { // actionAB:: a -> IO b
      return this.unit(this.run(actionAB(this.run(instanceA))));
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


// // unit:: a -> IO a
// var unit = (any) => {
//   return (world) =>  {  // 現在の外界
//     return pair.cons(any, world);
//   };
// };

// // flatMap:: IO a -> (a -> IO b) -> IO b
// var flatMap = (instanceA) => {
//   return (actionAB) => { // actionAB:: a -> IO b
//     return (world) => {
//       var newPair = instanceA(world); // 現在の外界のなかで instanceAのIOアクションを実行する
//       return pair.match(newPair,{
//         cons: (value, newWorld) => {
//           return actionAB(value)(newWorld); // 新しい外界のなかで、actionAB(value)で作られたIOアクションを実行する
//         }
//       });
//     };
//   };
// };

// // done:: T -> IO T
// var done = (any) => {
//   return unit();
// };

// // run:: io[A] -> A
// var run = (instance) => {
//   return (world) => {
//     var newPair = instance(world); // ioモナドのインスタンス(アクション)を現在の外界に適用する
//     return pair.left(newPair);
//   };
// };

// // println:: STRING => IO[null]
// var println = (message) => {
//   return (world) => {
//     console.log(message);
//     return unit(null)(world);
//   };
// };

// // return:: STRING => IO[STRING]
// var readFile = (path) => {
//   return (world) => {
//     var fs = require('fs');
//     var content = fs.readFileSync(path, 'utf8');
//     return unit(content)(world);
//   };
// };


// var writeFile = (content) => {
//   return (io) => {
//     console.log(content);
//     return null;
//   };
// };

// var readln = function(io) {
//   var fs = require('fs');
//   var length = 80;
//   var buffer = new Buffer(length);
//   fs.readSync(process.stdin.fd, buffer, 0, length)
//   var line = buffer.split(/\r?\n/);
//   return line;
// };

// var initialWorld = true;

// io.run(io.println("test"))(initialWorld);
// io.run(io.readFile("./io_with_world.js"))(initialWorld);

// io.flatMap(io.readFile("./io_with_world.js"))((content) => {
//   return io.flatMap(io.println(content))((_) => {
//     return io.done(_);
//   });
// })(initialWorld);
