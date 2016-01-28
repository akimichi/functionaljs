"use strict";

var io = {
  // unit:: a -> IO a
  unit: (any) => {
    return (_) =>  {
      return any;
    };
  },
  // done:: T -> IO T
  done: (any) => {
    return io.unit();
  },
  // flatMap:: IO a -> (a -> IO b) -> IO b
  flatMap: (instanceA) => {
    return (actionAB) => { // actionAB:: a -> IO b
      return io.unit(io.run(actionAB(io.run(instanceA))));
    };
  },
  // run:: IO A -> A
  run: (instance) => {
    return instance();
  },
  readFile: (path) => {
    return (io) => {
      var fs = require('fs');
      var content = fs.readFileSync(path, 'utf8');
      return content;
    };
  },
  println: (message) => {
    return (io) => {
      console.log(message);
      return null;
    };
  }
};

var initialWorld = null;
var path = process.argv[2];
 
/* #@range_begin(io_monad_combined) */
io.flatMap(io.readFile(path))((content) => {
  return io.flatMap(io.println(content))((_) => {
    return io.done(_);
  });
})(initialWorld);
/* #@range_end(io_monad_combined) */
