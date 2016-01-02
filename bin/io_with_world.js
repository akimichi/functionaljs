"use strict";

// var pair = {
//   cons: (left, right) => {
//     return (pattern) => {
//       return pattern.cons(left, right);
//     };
//   },
//   match : (data, pattern) => {
//     return data.call(pair, pattern);
//   },
//   right: (tuple) => {
//     return pair.match(tuple, {
//       cons: (left, right) => {
//         return right;
//       }
//     });
//   },
//   left: (tuple) => {
//     return pair.match(tuple, {
//       cons: (left, right) => {
//         return left;
//       }
//     });
//   }
// };
var cons = (left, right) => {
  return (pattern) => {
    return pattern.cons(left, right);
  };
};
var match = (data, pattern) => {
  return data(pattern);
};
var right = (tuple) => {
  return match(tuple, {
    cons: (left, right) => {
      return right;
    }
  });
};

var left = (tuple) => {
  return match(tuple, {
    cons: (left, right) => {
      return left;
    }
  });
};



// unit:: a -> IO a
var unit = (any) => {
  return (world) =>  {  // 現在の外界
    // return [any, world];
    return cons(any, world);
  };
};

// done:: T -> IO T
var done = (any) => {
  return unit();
};

// run:: io[A] -> A
var run = (instance) => {
  return (world) => {
    var newPair = instance(world); // ioモナドのインスタンス(アクション)を現在の外界に適用する
    // return newPair[0];
    return match(newPair,{
      cons: (value, newWorld) => {
        return value;
      }
    });
  };
};

// flatMap:: IO a -> (a -> IO b) -> IO b
var flatMap = (instanceA) => {
  return (actionAB) => { // actionAB:: a -> IO b
    return (world) => {

      // var newPair = instanceA(world);
      // var value = newPair[0];
      // var newWorld = newPair[1];
      // return actionAB(value)(newWorld);

      var newPair = instanceA(world);
      return match(newPair,{
        cons: (value, newWorld) => {
          return actionAB(value)(newWorld);
        }
      });

      // return unit(run(actionAB(run(instanceA))));
      // return pair.match(run(instanceA)(world))({
      //   cons: (value, newWorld) => {
      //     return run(actionAB(value))(newWorld);
      //   }
      // });
    };
  };
};

// println:: STRING => IO[null]
var println = (message) => {
  return (world) => {
    console.log(message);
    return unit(null)(world);
  };
};

// return:: STRING => IO[STRING]
var readFile = (path) => {
  return (world) => {
    var fs = require('fs');
    var content = fs.readFileSync(path, 'utf8');
    return unit(content)(world);
  };
};

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

var initialWorld = true;

// run(println("test"));
// run(readFile(initialWorld)("./io_with_world.js"));
run(println("test"))(initialWorld);
run(readFile("./io_with_world.js"))(initialWorld);

flatMap(readFile("./io_with_world.js"))((content) => {
  return flatMap(println(content))((_) => {
    return done(_);
  });
})(initialWorld);
